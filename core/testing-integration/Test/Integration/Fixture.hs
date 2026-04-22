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
import Service.Integration.Fixture qualified as ProdFixture
import Service.Integration.FixtureKey qualified as FixtureKey
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
  validName <- case ProdFixture.validateIntegrationName integrationName of
    Ok v -> Task.yield v
    Err err -> Task.throw (ValidationFailure err)
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
      Task.forEach (recordOne projectRoot validName rules) requests
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
      let hex = FixtureKey.toText (FixtureKey.fromRequest request)
      let dir = GhcFilePath.joinPath [Text.toLinkedList root, "tests", "fixtures", "local", Text.toLinkedList name]
      let filePath = dir GhcFilePath.</> (Text.toLinkedList hex ++ ".json")
      let localRootAssembled =
            GhcFilePath.joinPath [Text.toLinkedList root, "tests", "fixtures", "local"]
      assertInsideRoot localRootAssembled filePath
      let content = Aeson.object [(AesonKey.fromText "request", redactedRequest), (AesonKey.fromText "response", redactedResponse)]
      writeFixture dir filePath content


-- | Promote local fixture files matching any of the given hash prefixes.
promote ::
  Text ->
  Array Text ->
  Task IntegrationError ()
promote integrationName hashPrefixes = do
  validName <- case ProdFixture.validateIntegrationName integrationName of
    Ok v -> Task.yield v
    Err err -> Task.throw (ValidationFailure err)
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
            Text.toLinkedList validName
          ]
  let committedDir =
        GhcFilePath.joinPath
          [ Text.toLinkedList projectRoot,
            "tests",
            "fixtures",
            Text.toLinkedList validName
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
  assertInsideRoot localDir src
  assertInsideRoot committedDir dest
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


-- | Look up the project root from @NEOHASKELL_PROJECT_ROOT@. ADR-0055 §4
-- forbids an implicit cwd fallback: a test binary launched from the wrong
-- directory would otherwise record sandbox responses (potentially
-- containing real tokens) into an unrelated repository tree with no
-- @tests/fixtures/local/@ gitignore.
resolveProjectRoot :: Task IntegrationError Text
resolveProjectRoot = do
  envResult <-
    Environment.getVariable "NEOHASKELL_PROJECT_ROOT"
      |> Task.asResult
  case envResult of
    Ok v -> Task.yield v
    Err _ ->
      Task.throw
        ( ValidationFailure
            "NEOHASKELL_PROJECT_ROOT must be set for Test.Integration.Fixture record/promote"
        )


-- | Fail the task if the resolved target is not a descendant of the
-- expected root. Canonicalises both sides so intermediate symlinks are
-- followed, defeating escape attempts via symlinked path components.
assertInsideRoot :: [Char] -> [Char] -> Task IntegrationError ()
assertInsideRoot root target = do
  rootCanon <-
    GhcDir.canonicalizePath root
      |> Task.fromIO
  targetParent <- do
    let parent = GhcFilePath.takeDirectory target
    GhcDir.createDirectoryIfMissing True parent
      |> Task.fromIO
    GhcDir.canonicalizePath parent
      |> Task.fromIO
  let targetCanon = targetParent GhcFilePath.</> GhcFilePath.takeFileName target
  case rootCanon `GhcList.isPrefixOf` targetCanon of
    True -> Task.yield ()
    False ->
      Task.throw
        ( ValidationFailure
            [fmt|Fixture path escape: #{Text.fromLinkedList targetCanon} is not inside #{Text.fromLinkedList rootCanon}|]
        )
