{-# LANGUAGE Strict #-}

-- | Owns the one integration_result log record produced for each dispatch result.
module Service.Integration.Dispatcher.IntegrationResult (
  RunnerKind (..),
  IntegrationResult (..),
  logFields,
  logLevel,
  logMessage,
) where

import Array (Array)
import Array qualified
import Basics
import Json qualified
import Log qualified
import Service.Integration.Dispatcher.IntegrationOutcome (IntegrationOutcome (..))
import Service.Integration.Dispatcher.IntegrationOutcome qualified as IntegrationOutcome
import Text (Text)


data RunnerKind
  = OutboundKind
  | LifecycleKind


data IntegrationResult
  = Skipped
  | Dropped
  | RunnerResult RunnerKind Int IntegrationOutcome Int


logFields :: IntegrationResult -> Array (Text, Json.Value)
logFields result =
  Array.fromLinkedList
    [ ("kind" Json..= ("integration_result" :: Text)),
      ("result" Json..= resultLabel result),
      ("commandsEmitted" Json..= commandCount result)
    ]
    |> Array.append (runnerFields result)


logLevel :: IntegrationResult -> Log.Level
logLevel result = case result of
  Skipped -> Log.Info
  Dropped -> Log.Warn
  RunnerResult _ _ outcome _ -> case outcome of
    Processed -> Log.Info
    Failed _ -> Log.Warn


logMessage :: IntegrationResult -> Text
logMessage result = case result of
  Skipped -> "No integration runner matched the event"
  Dropped -> "Integration event was not delivered"
  RunnerResult _ _ outcome _ -> IntegrationOutcome.message outcome


resultLabel :: IntegrationResult -> Text
resultLabel result = case result of
  Skipped -> "skipped"
  Dropped -> "dropped"
  RunnerResult _ _ outcome _ -> IntegrationOutcome.resultLabel outcome


commandCount :: IntegrationResult -> Int
commandCount result = case result of
  RunnerResult _ _ _ count -> count
  _ -> 0


runnerFields :: IntegrationResult -> Array (Text, Json.Value)
runnerFields result = case result of
  RunnerResult kind ordinal outcome _ ->
    Array.fromLinkedList
      [ ("runnerKind" Json..= kindLabel kind),
        ("runnerOrdinal" Json..= ordinal)
      ]
      |> Array.append (IntegrationOutcome.failureFields outcome)
  _ -> Array.empty


kindLabel :: RunnerKind -> Text
kindLabel kind = case kind of
  OutboundKind -> "outbound"
  LifecycleKind -> "lifecycle"
