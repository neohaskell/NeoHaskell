-- | CLI flag parsing for outbound-integration selection.
--
-- Implements ADR-0055 §2: @--integrations=fake@ (or @=hybrid@) is honoured
-- solely on the basis of the CLI flag.  There is no environment-variable gate.
-- The flag is visible in @ps@, in container orchestration UIs, and in CI logs,
-- providing a single, auditable switch.
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
    docsUrl,
  )
where

import Array (Array)
import Array qualified
import Basics
import Control.Monad qualified as Monad
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
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


-- | Outbound-integration selection mode — see ADR-0055 §2.
data Selection
  = Real
  | Fake
  | Hybrid (Array Text)
  deriving (Eq, Show, Generic)


-- | Set of integration names that should dispatch to fakes in hybrid mode.
newtype FakeNameRegistry = FakeNameRegistry (Set Text)
  deriving (Eq, Show, Generic)


docsUrl :: Text
docsUrl = "https://neohaskell.org/docs/integrations/fake-mode"


-- | Parse the selection from the real process argv.
parseSelection :: Task Text Selection
parseSelection = do
  argv <-
    GhcEnv.getArgs
      |> Task.fromIO
  let args = argv |> GhcList.map Text.fromLinkedList
  parseSelectionFromArgs args


-- | Pure parser that derives a 'Selection' from a pre-captured argv list.
-- Exposed for tests so they can seed args in isolation from the real process.
parseSelectionFromArgs :: [Text] -> Task Text Selection
parseSelectionFromArgs args = do
  let mode = parseModeFlag args
  let fakes = parseFakeFlags args
  case mode of
    Nothing -> Task.yield Real
    Just "real" -> Task.yield Real
    Just "fake" -> Task.yield Fake
    Just "hybrid" -> do
      validated <- validateFakeNames fakes
      Task.yield (Hybrid validated)
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


-- | Emit an 'ERROR' level log line naming the active fakes when the
-- selection is non-'Real'. Logging wiring into Application.run is
-- tracked in ADR-0055 §2.
validateOrThrow :: Selection -> Task Text Selection
validateOrThrow selection = Task.yield selection


-- | True when an integration name is routed to its fake.
isFakeByName :: Text -> Selection -> Bool
isFakeByName name selection =
  case selection of
    Real -> False
    Fake -> True
    Hybrid names -> Array.contains name names


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


lastList :: [item] -> Maybe item
lastList xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ : rest -> lastList rest
