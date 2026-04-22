-- | Production-safe fixture *lookup* (read-only path).
--
-- ADR-0055 §4 requires:
--
-- * Hash-keyed JSON files under @<projectRoot>/tests/fixtures/<name>/<hash>.json@.
-- * Integration names validated against @[A-Za-z0-9_]+@.
-- * Symlink and @..@ escape rejected via 'System.Directory.makeAbsolute'
--   prefix checks.
module Service.Integration.Fixture
  ( FixtureMiss (..),
    lookupResponseBytes,
    lookupResponse,
    resolvePath,
    validateIntegrationName,
    fixturesRoot,
    fixturePath,
  )
where

import Basics
import Control.Exception qualified as GhcException
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.Either qualified as GhcEither
import Data.ByteString qualified as ByteString
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
import Result (Result (..))
import Service.Integration.FixtureKey (FixtureKey)
import Service.Integration.FixtureKey qualified as FixtureKey
import System.Directory qualified as GhcDir
import System.FilePath (FilePath)
import System.FilePath qualified as GhcFilePath
import System.IO.Error qualified as GhcIOError
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Prelude ((++), (.))


-- | Reasons a fixture lookup may miss.
data FixtureMiss
  = FixtureFileAbsent
  | FixtureOutsideRoot
  | FixtureNameInvalid Text
  | FixtureDecodeError Text
  deriving (Eq, Show, Generic)


-- | Validate an integration name against the regex @[A-Za-z0-9_]+@.
validateIntegrationName :: Text -> Result Text Text
validateIntegrationName name =
  if Text.isEmpty name
    then Err [fmt|Integration name '#{name}' must match [A-Za-z0-9_]+|]
    else do
      let chars = Text.toLinkedList name
      if GhcList.all isNameChar chars
        then Ok name
        else Err [fmt|Integration name '#{name}' must match [A-Za-z0-9_]+|]
  where
    isNameChar c =
      GhcChar.isAsciiUpper c
        || GhcChar.isAsciiLower c
        || GhcChar.isDigit c
        || c == '_'


-- | Compute the @tests/fixtures/@ directory beneath a project root.
fixturesRoot :: Text -> Text
fixturesRoot projectRoot =
  [fmt|#{projectRoot}/tests/fixtures|]


-- | The final fixture file path for a given root/name/key triple. Does no
-- filesystem access — purely textual.
fixturePath :: Text -> Text -> FixtureKey -> Text
fixturePath projectRoot name key = do
  let hex = FixtureKey.toText key
  [fmt|#{fixturesRoot projectRoot}/#{name}/#{hex}.json|]


-- | Resolve a fixture file path safely: validates the integration name,
-- assembles the target, canonicalises with 'GhcDir.makeAbsolute', and
-- prefix-checks against the canonicalised fixtures root.
resolvePath :: Text -> Text -> FixtureKey -> Task Text Text
resolvePath projectRoot name key = do
  validatedName <- case validateIntegrationName name of
    Ok v -> Task.yield v
    Err err -> Task.throw [fmt|Fixture path rejected: #{err}|]
  case validateProjectRoot projectRoot of
    Err err -> Task.throw err
    Ok () -> do
      let assembled = pathJoin [Text.toLinkedList projectRoot, "tests", "fixtures", Text.toLinkedList validatedName, Text.toLinkedList (FixtureKey.toText key) ++ ".json"]
      let rootAssembled = pathJoin [Text.toLinkedList projectRoot, "tests", "fixtures"]
      absolute <-
        makeAbsoluteSafe assembled
          |> Task.mapError (\err -> [fmt|Fixture path resolve failed: #{err}|])
      rootAbs <-
        makeAbsoluteSafe rootAssembled
          |> Task.mapError (\err -> [fmt|Project root resolve failed: #{err}|])
      if not (rootAbs `GhcList.isPrefixOf` absolute)
        then Task.throw [fmt|Fixture path escapes project root: #{Text.fromLinkedList absolute} is not a descendant of #{Text.fromLinkedList rootAbs}|]
        else Task.yield (Text.fromLinkedList absolute)


validateProjectRoot :: Text -> Result Text ()
validateProjectRoot root =
  if Text.isEmpty root
    then Err "Fixture project root not set — refusing to resolve"
    else Ok ()


-- | Read raw fixture bytes. Returns 'FixtureFileAbsent' on miss,
-- 'FixtureOutsideRoot' when the final path fails the prefix check or points
-- via a symlink outside the fixtures tree.
lookupResponseBytes ::
  Text ->
  Text ->
  FixtureKey ->
  Task Text (Result FixtureMiss ByteString.ByteString)
lookupResponseBytes projectRoot name key = do
  case validateIntegrationName name of
    Err err -> Task.yield (Err (FixtureNameInvalid err))
    Ok validName ->
      case validateProjectRoot projectRoot of
        Err err -> Task.throw err
        Ok () -> do
          let assembled = pathJoin [Text.toLinkedList projectRoot, "tests", "fixtures", Text.toLinkedList validName, Text.toLinkedList (FixtureKey.toText key) ++ ".json"]
          let rootAssembled = pathJoin [Text.toLinkedList projectRoot, "tests", "fixtures"]
          absoluteResult <- Task.asResult (makeAbsoluteSafe assembled)
          case absoluteResult of
            Err _ -> Task.yield (Err FixtureFileAbsent)
            Ok absolute -> do
              rootResult <- Task.asResult (makeAbsoluteSafe rootAssembled)
              case rootResult of
                Err err -> Task.throw err
                Ok rootAbs ->
                  if not (rootAbs `GhcList.isPrefixOf` absolute)
                    then Task.yield (Err FixtureOutsideRoot)
                    else do
                      symlinkRes <-
                        symlinkInsideRoot rootAbs absolute
                          |> Task.asResult
                      case symlinkRes of
                        Ok True -> do
                          exists <-
                            GhcDir.doesFileExist absolute
                              |> Task.fromIO
                          if not exists
                            then Task.yield (Err FixtureFileAbsent)
                            else do
                              bytes <-
                                ByteString.readFile absolute
                                  |> Task.fromFailableIO @GhcException.IOException
                                  |> Task.mapError (Text.fromLinkedList . GhcException.displayException)
                              Task.yield (Ok bytes)
                        Ok False -> Task.yield (Err FixtureOutsideRoot)
                        Err _ -> Task.yield (Err FixtureOutsideRoot)


-- | Decode a fixture file's @response@ field.
lookupResponse ::
  forall response.
  (Aeson.FromJSON response) =>
  Text ->
  Text ->
  FixtureKey ->
  Task Text (Result FixtureMiss response)
lookupResponse projectRoot name key = do
  bytesResult <- lookupResponseBytes projectRoot name key
  case bytesResult of
    Err miss -> Task.yield (Err miss)
    Ok bytes -> do
      case Aeson.eitherDecodeStrict bytes of
        GhcEither.Left err -> Task.yield (Err (FixtureDecodeError (Text.fromLinkedList err)))
        GhcEither.Right (val :: Aeson.Value) ->
          case extractResponse val of
            Err err -> Task.yield (Err (FixtureDecodeError err))
            Ok responseVal ->
              case Aeson.fromJSON responseVal of
                Aeson.Error err -> Task.yield (Err (FixtureDecodeError (Text.fromLinkedList err)))
                Aeson.Success parsed -> Task.yield (Ok parsed)


extractResponse :: Aeson.Value -> Result Text Aeson.Value
extractResponse val =
  case AesonTypes.parse responseParser val of
    Aeson.Error err -> Err (Text.fromLinkedList err)
    Aeson.Success v -> Ok v
  where
    responseParser :: Aeson.Value -> AesonTypes.Parser Aeson.Value
    responseParser = Aeson.withObject "fixture" \obj -> obj Aeson..: "response"


-- Filesystem helpers -------------------------------------------------------

-- | Canonicalise a path via 'GhcDir.canonicalizePath' — this resolves every
-- intermediate symbolic link in the path, not just the final component
-- (which is what 'GhcDir.makeAbsolute' would do). Combined with the
-- subsequent prefix check against the canonicalised project root, this
-- blocks escape attempts via symlinked parent directories.
makeAbsoluteSafe :: FilePath -> Task Text FilePath
makeAbsoluteSafe p =
  GhcDir.canonicalizePath p
    |> Task.fromFailableIO @GhcIOError.IOError
    |> Task.mapError (Text.fromLinkedList . show)


-- | When a file is a symlink, check that its ultimate target still lies
-- inside the fixtures root.
symlinkInsideRoot :: FilePath -> FilePath -> Task Text Bool
symlinkInsideRoot rootAbs absolute = do
  isSymlink <-
    GhcDir.pathIsSymbolicLink absolute
      |> Task.fromFailableIO @GhcIOError.IOError
      |> Task.mapError (Text.fromLinkedList . show)
      |> Task.recover (\_ -> Task.yield False)
  case isSymlink of
    False -> Task.yield True
    True -> do
      target <-
        GhcDir.getSymbolicLinkTarget absolute
          |> Task.fromFailableIO @GhcIOError.IOError
          |> Task.mapError (Text.fromLinkedList . show)
      -- Resolve the target against the symlink directory.
      let parent = GhcFilePath.takeDirectory absolute
      let joined =
            if GhcFilePath.isAbsolute target
              then target
              else parent GhcFilePath.</> target
      targetAbs <- makeAbsoluteSafe joined
      Task.yield (rootAbs `GhcList.isPrefixOf` targetAbs)


pathJoin :: [FilePath] -> FilePath
pathJoin = GhcList.foldl1 (GhcFilePath.</>)
