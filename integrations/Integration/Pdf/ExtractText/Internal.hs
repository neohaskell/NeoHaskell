{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for PDF text extraction integration.
--
-- This module contains Nick's code - the 'ToAction' instance,
-- subprocess execution, temp file handling, and metadata parsing.
--
-- __This module is not exported to Jess.__
module Integration.Pdf.ExtractText.Internal
  ( -- * For Testing Only
    buildPdftotextArgs
  , parsePdfInfo
  , extractPageCount
  ) where

import Array (Array)
import Array qualified
import Basics
import File qualified
import Integration qualified
import Integration.Pdf.ExtractText (Config (..), Encoding (..), ExtractionResult (..), Layout (..), PdfMetadata (..), Request (..))
import Json qualified
import Maybe (Maybe (..))
import Path (Path, path)
import Path qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.FileUpload.Core (FileAccessError (..))
import Subprocess qualified
import System.Directory qualified as GhcDir
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid qualified


-- | ToAction instance that executes the PDF extraction.
--
-- This is the main entry point - when Jess writes:
--
-- @
-- Integration.outbound PdfExtract.Request { ... }
-- @
--
-- This instance converts the config into an executable action.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config = Integration.action \ctx -> do
    executeExtraction ctx config


-- | Execute a PDF extraction request.
executeExtraction ::
  forall command.
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ActionContext ->
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeExtraction ctx config = do
  -- Step 1: Get file access context
  fileAccess <- case ctx.fileAccess of
    Nothing ->
      Task.throw (Integration.ValidationError "File uploads not enabled. Cannot access PDF file.")
    Just fa ->
      Task.yield fa

  -- Step 2: Check if pdftotext is available
  pdftotextPath <- Subprocess.which "pdftotext"
    |> Task.mapError subprocessToIntegrationError
  case pdftotextPath of
    Nothing ->
      Task.throw (Integration.ValidationError "pdftotext not found. Please install poppler-utils.")
    Just _ ->
      pass

  -- Step 3: Retrieve file bytes
  pdfBytes <- fileAccess.retrieveFile config.fileRef
    |> Task.mapError fileAccessToIntegrationError

  -- Step 4: Write to temp file
  tempDir <- GhcDir.getTemporaryDirectory
    |> Task.fromIO
  tempId <- Uuid.generate
  let tempIdText = Uuid.toText tempId
  let tempFileName = [fmt|neohaskell-pdf-#{tempIdText}.pdf|]
  tempPath <- case Path.fromLinkedList tempDir of
    Nothing -> Task.throw (Integration.UnexpectedError "Invalid temp directory path")
    Just basePath -> case Path.fromText tempFileName of
      Nothing -> Task.throw (Integration.UnexpectedError "Invalid temp file name")
      Just fileName -> Task.yield (Path.append fileName basePath)
  File.writeBytes tempPath pdfBytes
    |> Task.mapError (\_ -> Integration.UnexpectedError "Failed to write temp file")

  -- Step 5: Execute pdftotext
  extractionResult <- executePdftotext tempPath config.config
    |> Task.asResult

  -- Step 6: Get metadata (optional, don't fail if it fails)
  metadataResult <- executePdfinfo tempPath
    |> Task.asResult

  -- Step 7: Clean up temp file
  File.deleteIfExists tempPath
    |> Task.mapError (\_ -> Integration.UnexpectedError "Failed to delete temp file")
    |> Task.recover (\_ -> Task.yield ())  -- Ignore cleanup errors

  -- Step 8: Emit result
  case extractionResult of
    Ok (extractedText, pageCount) -> do
      let metadata = case metadataResult of
            Ok m -> Just m
            Err _ -> Nothing
      let result = ExtractionResult
            { text = extractedText
            , pageCount = pageCount
            , metadata = metadata
            }
      Integration.emitCommand (config.onSuccess result)
    Err integrationError -> do
      let errorText = integrationErrorToText integrationError
      Integration.emitCommand (config.onError errorText)


-- | Execute pdftotext and return extracted text and page count.
executePdftotext ::
  Path ->
  Config ->
  Task Integration.IntegrationError (Text, Int)
executePdftotext inputPath config = do
  let args = buildPdftotextArgs config inputPath
  result <- Subprocess.runWithTimeout config.timeoutSeconds "pdftotext" args [path|.|]
    |> Task.mapError subprocessToIntegrationError

  case result.exitCode of
    0 -> do
      -- pdftotext outputs to stdout when using "-" as output
      let extractedText = result.stdout
      -- Extract page count from the PDF (we'll get it from pdfinfo)
      -- For now, return 0 and let pdfinfo fill it in
      Task.yield (extractedText, 0)
    _ -> do
      let stderrText = result.stderr
      Task.throw (Integration.PermanentFailure [fmt|pdftotext failed: #{stderrText}|])


-- | Build pdftotext command arguments.
buildPdftotextArgs :: Config -> Path -> Array Text
buildPdftotextArgs config inputPath = do
  let layoutArgs = case config.layout of
        PreserveLayout -> Array.fromLinkedList ["-layout"]
        RawText -> Array.empty
        Table -> Array.fromLinkedList ["-table"]

  let encodingArgs = case config.encoding of
        UTF8 -> Array.fromLinkedList ["-enc", "UTF-8"]
        Latin1 -> Array.fromLinkedList ["-enc", "Latin1"]
        ASCII -> Array.fromLinkedList ["-enc", "ASCII7"]

  let pageArgs = case config.pageRange of
        Nothing -> Array.empty
        Just (first, lastPage) ->
          Array.fromLinkedList ["-f", Text.fromInt first, "-l", Text.fromInt lastPage]

  let inputArg = Path.toText inputPath
  let outputArg = "-"  -- Output to stdout
  let finalArgs = Array.fromLinkedList [inputArg, outputArg]

  -- Build args as a flat array using Array.append
  layoutArgs
    |> Array.append encodingArgs
    |> Array.append pageArgs
    |> Array.append finalArgs


-- | Execute pdfinfo to get metadata.
executePdfinfo ::
  Path ->
  Task Integration.IntegrationError PdfMetadata
executePdfinfo inputPath = do
  let args = [Path.toText inputPath]
  result <- Subprocess.runWithTimeout 10 "pdfinfo" args [path|.|]
    |> Task.mapError subprocessToIntegrationError

  case result.exitCode of
    0 ->
      Task.yield (parsePdfInfo result.stdout)
    _ -> do
      let stderrText = result.stderr
      Task.throw (Integration.NetworkError [fmt|pdfinfo failed: #{stderrText}|])


-- | Parse pdfinfo output into PdfMetadata.
parsePdfInfo :: Text -> PdfMetadata
parsePdfInfo output = do
  let allLines = Text.lines output
  let findField :: Text -> Maybe Text
      findField name = do
        let prefix = Text.append name ":"
        let prefixLen = Text.length prefix
        allLines
          |> Array.find (\line -> Text.startsWith prefix line)
          |> fmap (\line -> Text.dropLeft prefixLen line |> Text.trim)
  PdfMetadata
    { title = findField "Title"
    , author = findField "Author"
    , creationDate = findField "CreationDate"
    , pageSize = findField "Page size"
    }


-- | Extract page count from pdfinfo output.
extractPageCount :: Text -> Int
extractPageCount output = do
  let allLines = Text.lines output
  let maybeLine = allLines
        |> Array.find (\line -> Text.startsWith "Pages:" line)
  case maybeLine of
    Nothing -> 0
    Just line -> do
      -- "Pages:" is 6 chars
      let countText = Text.dropLeft 6 line |> Text.trim
      case Text.toInt countText of
        Nothing -> 0
        Just n -> n


-- | Convert FileAccessError to IntegrationError.
fileAccessToIntegrationError :: FileAccessError -> Integration.IntegrationError
fileAccessToIntegrationError err = case err of
  FileNotFound _ ->
    Integration.ValidationError "File not found"
  StateLookupFailed _ msg ->
    Integration.UnexpectedError [fmt|Failed to lookup file state: #{msg}|]
  NotOwner _ ->
    Integration.AuthenticationError "Not authorized to access this file"
  FileExpired _ ->
    Integration.ValidationError "File has expired"
  FileIsDeleted _ ->
    Integration.ValidationError "File has been deleted"
  BlobMissing _ ->
    Integration.UnexpectedError "File blob is missing from storage"
  StorageError msg ->
    Integration.UnexpectedError [fmt|Storage error: #{msg}|]


-- | Convert Subprocess.Error to IntegrationError.
subprocessToIntegrationError :: Subprocess.Error -> Integration.IntegrationError
subprocessToIntegrationError err = case err of
  Subprocess.ProcessError msg ->
    Integration.UnexpectedError [fmt|Process error: #{msg}|]
  Subprocess.TimeoutError _ ->
    Integration.RateLimited 0  -- Timeout, could retry
  Subprocess.ToolNotFound msg ->
    Integration.ValidationError [fmt|Tool not found: #{msg}|]


-- | Convert IntegrationError to Text for the onError callback.
integrationErrorToText :: Integration.IntegrationError -> Text
integrationErrorToText err = case err of
  Integration.ValidationError msg -> msg
  Integration.AuthenticationError msg -> msg
  Integration.NetworkError msg -> msg
  Integration.RateLimited _ -> "Rate limited"
  Integration.PermanentFailure msg -> msg
  Integration.UnexpectedError msg -> msg
