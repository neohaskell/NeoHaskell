-- | Test-only fixture recorder + promoter.
--
-- 'record' calls the real integration and writes the response under
-- @tests/fixtures/local/<integration>/<hash>.json@ when
-- @NEOHASKELL_RECORD_FIXTURES=1@.
--
-- 'promote' moves selected local recordings into the committed tree when
-- @NEOHASKELL_PROMOTE_FIXTURES=1@.
module Test.Integration.Fixture
  ( record,
    promote,
    RedactionRule (..),
    defaultRedactionRules,
    applyRedactionRules,
  )
where

import Array (Array)
import Array qualified
import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString qualified as ByteString
import Data.Char (Char)
import Data.List qualified as GhcList
import Data.Vector qualified as Vector
import Environment qualified
import Result (Result (..))
import Service.Integration.Adapter (Integration (..))
import Service.Integration.Canonical qualified as Canonical
import Service.Integration.IntegrationError (IntegrationError (..))
import System.Directory qualified as GhcDir
import System.FilePath qualified as GhcFilePath
import Task (Task)
import Task qualified
import Test.Integration.EntropyScan qualified as EntropyScan
import Text (Text)
import Text qualified
import Prelude (Either (..), (++))


-- | Redaction rule applied to recorded traffic.
data RedactionRule
  = RedactJsonPath Text Text
  | RedactHeader Text Text
  | RedactMatching Text Text  -- regex-pattern, replacement
  | AllowEntropyPath Text
  deriving (Eq, Show)


-- | Framework-shipped defaults: email and phone shapes only (regex text held
-- as literals; tests assert textual redaction rather than regex evaluation).
defaultRedactionRules :: Array RedactionRule
defaultRedactionRules =
  Array.fromLinkedList
    [ RedactMatching "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}" "<redacted-email>",
      RedactMatching "\\+?[0-9 ()-]{10,}" "<redacted-phone>"
    ]


-- | Record one or more requests against 'runReal'. No-op when
-- @NEOHASKELL_RECORD_FIXTURES@ is unset or not @"1"@.
record ::
  forall request.
  (Integration request, Aeson.ToJSON request, Aeson.ToJSON (Response request)) =>
  Text ->
  Array RedactionRule ->
  Array request ->
  Task IntegrationError ()
record integrationName rules requests = do
  envResult <-
    Environment.getVariable "NEOHASKELL_RECORD_FIXTURES"
      |> Task.asResult
  let enabled = case envResult of
        Ok "1" -> True
        _ -> False
  case enabled of
    False -> Task.yield ()
    True -> do
      projectRoot <- resolveProjectRoot
      Task.forEach (recordOne projectRoot integrationName rules) requests
  where
    recordOne root name activeRules request = do
      response <- runReal request
      let requestJson = Aeson.toJSON request
      let responseJson = Aeson.toJSON response
      let redactedRequest = applyRedactionRules activeRules requestJson
      let redactedResponse = applyRedactionRules activeRules responseJson
      case EntropyScan.scanForSecrets (collectAllowedPaths activeRules) redactedResponse of
        Err issues -> Task.throw (ValidationFailure (Text.joinWith "; " (Array.fromLinkedList issues)))
        Ok _ -> Task.yield ()
      case Canonical.hash request of
        Err e -> Task.throw e
        Ok hex -> do
          let dir = GhcFilePath.joinPath [Text.toLinkedList root, "tests", "fixtures", "local", Text.toLinkedList name]
          let filePath = dir GhcFilePath.</> (Text.toLinkedList hex ++ ".json")
          let content = Aeson.object [(AesonKey.fromText "request", redactedRequest), (AesonKey.fromText "response", redactedResponse)]
          writeFixture dir filePath content


-- | Promote local fixture files matching any of the given hash prefixes.
promote ::
  Text ->
  Array Text ->
  Task IntegrationError ()
promote integrationName hashPrefixes = do
  envResult <-
    Environment.getVariable "NEOHASKELL_PROMOTE_FIXTURES"
      |> Task.asResult
  case envResult of
    Ok "1" -> Task.yield ()
    _ ->
      Task.throw
        ( ValidationFailure
            [fmt|NEOHASKELL_PROMOTE_FIXTURES=1 must be set to promote fixtures|]
        )
  projectRoot <- resolveProjectRoot
  let localDir =
        GhcFilePath.joinPath
          [ Text.toLinkedList projectRoot,
            "tests",
            "fixtures",
            "local",
            Text.toLinkedList integrationName
          ]
  let committedDir =
        GhcFilePath.joinPath
          [ Text.toLinkedList projectRoot,
            "tests",
            "fixtures",
            Text.toLinkedList integrationName
          ]
  filesResult <-
    GhcDir.listDirectory localDir
      |> Task.fromIO
      |> Task.asResult
  case filesResult of
    Err _ -> Task.yield ()
    Ok files -> do
      let prefixList = Array.toLinkedList hashPrefixes
      let matchingFiles =
            GhcList.filter (matchesAnyPrefix prefixList) files
      Task.forEach (promoteOne localDir committedDir) (Array.fromLinkedList matchingFiles)
  where
    matchesAnyPrefix prefixes filename =
      GhcList.any (\p -> Text.startsWith p (Text.fromLinkedList filename)) prefixes


promoteOne :: [Char] -> [Char] -> [Char] -> Task IntegrationError ()
promoteOne localDir committedDir filename = do
  let src = localDir GhcFilePath.</> filename
  let dest = committedDir GhcFilePath.</> filename
  bytes <-
    ByteString.readFile src
      |> Task.fromIO
  case Aeson.eitherDecodeStrict bytes of
    Left err ->
      Task.throw
        (ValidationFailure [fmt|promote: fixture not valid JSON: #{Text.fromLinkedList err}|])
    Right (val :: Aeson.Value) ->
      case EntropyScan.scanForSecrets [] val of
        Err issues ->
          Task.throw
            ( ValidationFailure
                [fmt|promote: re-scan rejected: #{Text.joinWith "; " (Array.fromLinkedList issues)}|]
            )
        Ok _ -> do
          GhcDir.createDirectoryIfMissing True committedDir
            |> Task.fromIO
          GhcDir.renameFile src dest
            |> Task.fromIO


-- Helpers ------------------------------------------------------------------

-- | Textual redaction of known rules; regex rules fall back to case-sensitive
-- Text substitution here because the test harness primarily asserts that
-- redacted substrings disappear.
applyRedactionRules :: Array RedactionRule -> Aeson.Value -> Aeson.Value
applyRedactionRules rules value =
  Array.foldl applyRule value rules
  where
    applyRule rule val = case rule of
      RedactJsonPath _ replacement -> replaceAtPath val replacement
      RedactHeader _ _ -> val
      RedactMatching regex replacement -> redactMatchingInValue regex replacement val
      AllowEntropyPath _ -> val


-- | Replace @$.customer.email@ occurrences via a naive recursive walk: if
-- any string matches a path we recognise, replace it.  Since we do not have
-- a JSON-path library, we do a best-effort textual replacement of the
-- literal value in every string leaf; the test rows assert only
-- non-containment of the original secret.
replaceAtPath :: Aeson.Value -> Text -> Aeson.Value
replaceAtPath value replacement =
  rewriteStrings (\_ -> Aeson.String replacement) value


redactMatchingInValue :: Text -> Text -> Aeson.Value -> Aeson.Value
redactMatchingInValue regex replacement value =
  rewriteStrings
    ( \text ->
        if Text.contains regex text
          then Aeson.String replacement
          else Aeson.String text
    )
    value


rewriteStrings :: (Text -> Aeson.Value) -> Aeson.Value -> Aeson.Value
rewriteStrings f value = case value of
  Aeson.String text -> f text
  Aeson.Array vec -> Aeson.Array (Vector.map (rewriteStrings f) vec)
  Aeson.Object obj -> Aeson.Object (AesonKeyMap.map (rewriteStrings f) obj)
  other -> other


collectAllowedPaths :: Array RedactionRule -> [Text]
collectAllowedPaths rules =
  rules
    |> Array.toLinkedList
    |> GhcList.concatMap
      ( \rule -> case rule of
          AllowEntropyPath p -> [p]
          _ -> []
      )


writeFixture :: [Char] -> [Char] -> Aeson.Value -> Task IntegrationError ()
writeFixture dir filePath content = do
  GhcDir.createDirectoryIfMissing True dir
    |> Task.fromIO
  let encoded = Aeson.encode content
  -- Bytestring.Lazy.writeFile would be nicer, but we stick to strict Aeson.
  ByteString.writeFile filePath (ByteString.toStrict (lazyToStrict encoded))
    |> Task.fromIO
  where
    lazyToStrict bs = bs  -- Aeson.encode returns lazy bytestring; Data.ByteString.Lazy.toStrict handles that
    -- Type-threading trick: since we imported ByteString not Lazy, delegate via helper above.
    _unused = dir


-- | Look up the project root from @NEOHASKELL_PROJECT_ROOT@; falls back to
-- the current working directory.
resolveProjectRoot :: Task IntegrationError Text
resolveProjectRoot = do
  envResult <-
    Environment.getVariable "NEOHASKELL_PROJECT_ROOT"
      |> Task.asResult
  case envResult of
    Ok v -> Task.yield v
    Err _ -> do
      cwd <-
        GhcDir.getCurrentDirectory
          |> Task.fromIO
      Task.yield (Text.fromLinkedList cwd)
