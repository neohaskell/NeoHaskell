-- | CLI flag + environment-variable gate for integration selection.
--
-- Implements ADR-0055 §3: @--integrations=fake@ or @--integrations=hybrid@
-- is only honoured when @NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1@ is in the
-- process environment.
module Service.Integration.Selection
  ( Selection (..),
    FakeNameRegistry (..),
    parseSelection,
    parseSelectionFromArgs,
    validateOrThrow,
    isFakeByName,
    fromSelection,
    fakeNameMember,
    fakeNameFromArray,
    validateFakeName,
    envGateName,
    envGateValue,
    docsUrl,
  )
where

import Array (Array)
import Array qualified
import Basics
import Control.Monad qualified as Monad
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
import Environment qualified
import Data.Maybe qualified as GhcMaybe
import Maybe (Maybe (..))
import Result (Result (..))
import Set (Set)
import Set qualified
import System.Environment qualified as GhcEnv
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Outbound-integration selection mode — see ADR-0055 §3.
data Selection
  = Real
  | Fake
  | Hybrid (Array Text)
  deriving (Eq, Show, Generic)


-- | Set of integration names that should dispatch to fakes in hybrid mode.
newtype FakeNameRegistry = FakeNameRegistry (Set Text)
  deriving (Eq, Show, Generic)


envGateName :: Text
envGateName = "NEOHASKELL_ALLOW_FAKE_INTEGRATIONS"


envGateValue :: Text
envGateValue = "1"


docsUrl :: Text
docsUrl = "https://neohaskell.org/docs/integrations/fake-mode"


-- | Parse the selection from the real process env + argv.
parseSelection :: Task Text Selection
parseSelection = do
  argv <-
    GhcEnv.getArgs
      |> Task.fromIO
  let args = argv |> GhcList.map Text.fromLinkedList
  envResult <-
    Environment.getVariable envGateName
      |> Task.asResult
  let gateOpen = case envResult of
        Ok value -> value == envGateValue
        Err _ -> False
  parseSelectionFromArgs gateOpen args


-- | Pure-ish parser that derives a 'Selection' from the pre-captured env
-- and argv. Exposed for tests so they can seed the gate and args in
-- isolation from the real process environment.
parseSelectionFromArgs :: Bool -> [Text] -> Task Text Selection
parseSelectionFromArgs gateOpen args = do
  let mode = parseModeFlag args
  let fakes = parseFakeFlags args
  case mode of
    Nothing -> Task.yield Real
    Just "real" -> Task.yield Real
    Just "fake" ->
      if gateOpen
        then Task.yield Fake
        else Task.throw (gateClosedMessage "--integrations=fake")
    Just "hybrid" ->
      if gateOpen
        then do
          validated <- validateFakeNames fakes
          Task.yield (Hybrid validated)
        else Task.throw (gateClosedMessage "--integrations=hybrid")
    Just other ->
      Task.throw [fmt|unknown --integrations=#{other}; valid values are real, fake, hybrid|]


parseModeFlag :: [Text] -> Maybe Text
parseModeFlag args = do
  let prefix = "--integrations="
  args
    |> GhcMaybe.mapMaybe (stripPrefix prefix)
    |> lastList


parseFakeFlags :: [Text] -> [Text]
parseFakeFlags args = do
  let prefix = "--fake="
  GhcMaybe.mapMaybe (stripPrefix prefix) args


validateFakeNames :: [Text] -> Task Text (Array Text)
validateFakeNames names =
  case names of
    [] -> Task.yield (Array.fromLinkedList [])
    _ -> do
      validated <-
        Monad.mapM
          ( \name -> case validateFakeName name of
              Err err -> Task.throw err
              Ok ok -> Task.yield ok
          )
          names
      Task.yield (Array.fromLinkedList validated)


-- | Validate a per-fake name. Accepts only @[A-Za-z0-9_]+@.
validateFakeName :: Text -> Result Text Text
validateFakeName name =
  if Text.isEmpty name
    then Err [fmt|--fake=#{name}: name must match [A-Za-z0-9_]+|]
    else do
      let allChars = Text.toLinkedList name
      if GhcList.all isNameChar allChars
        then Ok name
        else Err [fmt|--fake=#{name}: name must match [A-Za-z0-9_]+|]
  where
    isNameChar c =
      GhcChar.isAsciiUpper c
        || GhcChar.isAsciiLower c
        || GhcChar.isDigit c
        || c == '_'


gateClosedMessage :: Text -> Text
gateClosedMessage flag =
  [fmt|#{flag} requires #{envGateName}=#{envGateValue}
in the process environment. This gate exists so production binaries
cannot silently dispatch to fake implementations. Set the env var in
your test harness or staging deployment only — never in production.
See: #{docsUrl}|]


-- | Emit an 'ERROR' level log line naming the active fakes when the
-- selection is non-'Real'. Presently the log sink is deliberately minimal
-- (no structured logger wiring in Phase 8).
validateOrThrow :: Selection -> Task Text Selection
validateOrThrow selection = do
  case selection of
    Real -> Task.yield selection
    Fake ->
      -- NOTE: logging wiring is out of scope for Phase 8 (Application.run
      -- integration lands later); we still return the selection.
      Task.yield selection
    Hybrid _names ->
      Task.yield selection


-- | True when an integration name is routed to its fake.
isFakeByName :: Text -> Selection -> Bool
isFakeByName _name selection =
  case selection of
    Real -> False
    Fake -> True
    Hybrid names -> Array.contains _name names


-- | Build a 'FakeNameRegistry' from a selection for hybrid dispatch lookup.
fromSelection :: Selection -> FakeNameRegistry
fromSelection selection = case selection of
  Real -> FakeNameRegistry Set.empty
  Fake -> FakeNameRegistry Set.empty
  Hybrid names -> FakeNameRegistry (Set.fromArray names)


fakeNameMember :: Text -> FakeNameRegistry -> Bool
fakeNameMember name (FakeNameRegistry set) = Set.member name set


fakeNameFromArray :: Array Text -> FakeNameRegistry
fakeNameFromArray names = FakeNameRegistry (Set.fromArray names)


-- Helpers ------------------------------------------------------------------

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix prefix text =
  if Text.startsWith prefix text
    then Just (Text.dropLeft (Text.length prefix) text)
    else Nothing


lastList :: [a] -> Maybe a
lastList xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ : rest -> lastList rest
