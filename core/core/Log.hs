module Log
  ( Level (..)
  , debug
  , info
  , warn
  , critical
  , -- Framework-internal
    withScope
  )
where

import Appendable ((++))
import Data.Semigroup qualified as GhcSemigroup
import Array (Array)
import Array qualified
import Basics
import Char (Char)
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import Control.Concurrent qualified as GhcThread
import Data.IORef qualified as GhcIORef
import Data.Text.Encoding qualified as GhcTextEncoding
import GHC.Stack qualified as Stack
import IO (IO)
import Json qualified
import LinkedList qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import System.Environment qualified as GhcEnv
import System.IO.Unsafe qualified as GhcUnsafe
import System.Log.FastLogger qualified as GhcLogger
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


data Level = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)


globalLogger :: (GhcLogger.TimedFastLogger, IO ())
globalLogger = GhcUnsafe.unsafePerformIO do
  timeCache <- GhcLogger.newTimeCache ("%Y-%m-%dT%H:%M:%S%z" :: GhcLogger.TimeFormat)
  GhcLogger.newTimedFastLogger timeCache (GhcLogger.LogStdout GhcLogger.defaultBufSize)
{-# NOINLINE globalLogger #-}


globalContext :: ConcurrentMap GhcThread.ThreadId (Map Text Text)
globalContext = GhcUnsafe.unsafePerformIO do
  (ConcurrentMap.new :: Task Text (ConcurrentMap GhcThread.ThreadId (Map Text Text))) |> Task.runOrPanic
{-# NOINLINE globalContext #-}


globalMinLevel :: GhcIORef.IORef Level
globalMinLevel = GhcUnsafe.unsafePerformIO do
  envLevel <- GhcEnv.lookupEnv "LOG_LEVEL"
  let level = case envLevel of
        Just "Debug" -> Debug
        Just "debug" -> Debug
        Just "DEBUG" -> Debug
        Just "Info" -> Info
        Just "info" -> Info
        Just "INFO" -> Info
        Just "Warn" -> Warn
        Just "warn" -> Warn
        Just "WARN" -> Warn
        Just "Error" -> Error
        Just "error" -> Error
        Just "ERROR" -> Error
        _ -> Info
  GhcIORef.newIORef level
{-# NOINLINE globalMinLevel #-}


withScope :: Array (Text, Text) -> Task err value -> Task err value
withScope fields task = do
  threadId <- GhcThread.myThreadId |> Task.fromIO
  existingContext <- ConcurrentMap.get threadId globalContext
  let baseContext = existingContext |> Maybe.withDefault Map.empty
  let newContext =
        fields
          |> Array.foldl (\(key, value) acc -> acc |> Map.set key value) baseContext
  ConcurrentMap.set threadId newContext globalContext
  Task.finally
    ( case existingContext of
        Just ctx -> ConcurrentMap.set threadId ctx globalContext
        Nothing -> ConcurrentMap.remove threadId globalContext
    )
    task


debug :: (Stack.HasCallStack) => Text -> Task _ Unit
debug message = writeLog Debug message
{-# INLINE debug #-}


info :: (Stack.HasCallStack) => Text -> Task _ Unit
info message = writeLog Info message
{-# INLINE info #-}


warn :: (Stack.HasCallStack) => Text -> Task _ Unit
warn message = writeLog Warn message
{-# INLINE warn #-}


critical :: (Stack.HasCallStack) => Text -> Task _ Unit
critical message = writeLog Error message
{-# INLINE critical #-}


writeLog :: (Stack.HasCallStack) => Level -> Text -> Task _ Unit
writeLog level message = do
  minLevel <- GhcIORef.readIORef globalMinLevel |> Task.fromIO
  Task.when (level >= minLevel) do
    Task.fromIO do
      let (timedLogger, _) = globalLogger
      let callSite =
            Stack.callStack
              |> Stack.getCallStack
              |> LinkedList.get 1
      threadId <- GhcThread.myThreadId
      maybeContext <-
        (ConcurrentMap.get threadId globalContext :: Task Text (Maybe (Map Text Text)))
          |> Task.runOrPanic
      timedLogger \formattedTime -> do
        let timeText =
              formattedTime
                |> GhcLogger.toLogStr
                |> GhcLogger.fromLogStr
                |> GhcTextEncoding.decodeUtf8
        let baseFields =
              [ "time" Json..= timeText
              , "level" Json..= toText level
              , "message" Json..= message
              ] :: [(Text, Json.Value)]
        let locationFields = case callSite of
              Just (_, loc) ->
                [ "file" Json..= (Text.fromLinkedList (Stack.srcLocFile loc))
                , "line" Json..= Stack.srcLocStartLine loc
                ] :: [(Text, Json.Value)]
              Nothing -> [] :: [(Text, Json.Value)]
        let contextFields = case maybeContext of
              Just ctx ->
                ctx
                  |> Map.entries
                  |> Array.map (\(key, value) -> key Json..= value)
                  |> Array.toLinkedList
              Nothing -> ([] :: [(Text, Json.Value)])
        let allFields = baseFields ++ locationFields ++ contextFields
        let entry = Json.object allFields
        let line = Json.encodeText entry
        let logLine = GhcLogger.toLogStr (Text.toLinkedList line)
        let newline = GhcLogger.toLogStr ("\n" :: [Char])
        logLine GhcSemigroup.<> newline
