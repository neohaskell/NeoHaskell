module Service.CommandExecutor.CommandErrorClass (
  CommandErrorClass (..),
  fromFailure,
  fromAccessError,
  fromEventStoreError,
  text,
) where

import Basics
import Service.AccessControl (AccessError (..))
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure (..))
import Service.Event qualified as Event
import Service.EventStore.Core qualified as EventStore
import Text (Text)


data CommandErrorClass
  = CommandRejectedClass
  | UnauthenticatedClass
  | ForbiddenClass
  | InsufficientPermissionsClass
  | StreamNotFoundClass
  | EventNotFoundClass
  | StorageFailureClass
  | SubscriptionNotFoundClass
  | SubscriptionErrorClass
  | TruncationErrorClass
  | ConsistencyCheckFailedClass
  | InsertionFailedClass
  | PayloadTooLargeClass
  | EmptyPayloadClass
  | ReadingAllErrorClass
  | RequestContextFailureClass
  | EntityFetchErrorClass
  | DecisionExecutionErrorClass
  | InvalidCommandEventsClass
  | SynchronousExceptionClass
  | ExecutionInterruptedClass
  deriving (Eq, Show)


fromFailure :: CommandExecutionFailure -> CommandErrorClass
fromFailure failure = case failure of
  RequestContextFailed _ -> RequestContextFailureClass
  EntityFetchFailed _ -> EntityFetchErrorClass
  RetryRefetchFailed _ -> EntityFetchErrorClass
  DecisionExecutionFailed _ -> DecisionExecutionErrorClass
  InvalidCommandEvents _ -> InvalidCommandEventsClass
  SynchronousExecutionFailed _ -> SynchronousExceptionClass


fromAccessError :: AccessError -> CommandErrorClass
fromAccessError accessError = case accessError of
  Unauthenticated -> UnauthenticatedClass
  Forbidden -> ForbiddenClass
  InsufficientPermissions _ -> InsufficientPermissionsClass


fromInsertionFailure :: Event.InsertionFailure -> CommandErrorClass
fromInsertionFailure insertionFailure = case insertionFailure of
  Event.ConsistencyCheckFailed -> ConsistencyCheckFailedClass
  Event.InsertionFailed _ -> InsertionFailedClass
  Event.PayloadTooLarge -> PayloadTooLargeClass
  Event.EmptyPayload -> EmptyPayloadClass


fromEventStoreError :: EventStore.Error -> CommandErrorClass
fromEventStoreError eventStoreError = case eventStoreError of
  EventStore.StreamNotFound _ _ -> StreamNotFoundClass
  EventStore.EventNotFound _ _ -> EventNotFoundClass
  EventStore.StorageFailure _ -> StorageFailureClass
  EventStore.SubscriptionNotFound _ -> SubscriptionNotFoundClass
  EventStore.SubscriptionError _ _ -> SubscriptionErrorClass
  EventStore.TruncationError _ _ _ _ -> TruncationErrorClass
  EventStore.InsertionError insertionFailure -> fromInsertionFailure insertionFailure
  EventStore.ReadingAllError _ -> ReadingAllErrorClass


text :: CommandErrorClass -> Text
text errorClass = case errorClass of
  CommandRejectedClass -> "CommandRejected"
  UnauthenticatedClass -> "Unauthenticated"
  ForbiddenClass -> "Forbidden"
  InsufficientPermissionsClass -> "InsufficientPermissions"
  StreamNotFoundClass -> "StreamNotFound"
  EventNotFoundClass -> "EventNotFound"
  StorageFailureClass -> "StorageFailure"
  SubscriptionNotFoundClass -> "SubscriptionNotFound"
  SubscriptionErrorClass -> "SubscriptionError"
  TruncationErrorClass -> "TruncationError"
  ConsistencyCheckFailedClass -> "ConsistencyCheckFailed"
  InsertionFailedClass -> "InsertionFailed"
  PayloadTooLargeClass -> "PayloadTooLarge"
  EmptyPayloadClass -> "EmptyPayload"
  ReadingAllErrorClass -> "ReadingAllError"
  RequestContextFailureClass -> "RequestContextFailure"
  EntityFetchErrorClass -> "EntityFetchError"
  DecisionExecutionErrorClass -> "DecisionExecutionError"
  InvalidCommandEventsClass -> "InvalidCommandEvents"
  SynchronousExceptionClass -> "SynchronousException"
  ExecutionInterruptedClass -> "ExecutionInterrupted"
