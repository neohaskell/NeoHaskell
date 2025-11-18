module Test.Service.EventStore.Core (BankAccountEvent (..), newInsertion) where

import Core
import Json qualified
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Task qualified
import Uuid qualified


-- | Example domain events for a bank account
data BankAccountEvent
  = AccountOpened {initialBalance :: Int}
  | MoneyDeposited {amount :: Int}
  | MoneyWithdrawn {amount :: Int}
  | AccountClosed
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON BankAccountEvent


instance Json.FromJSON BankAccountEvent


instance Default BankAccountEvent where
  def = AccountOpened {initialBalance = 0}


newInsertion :: Int -> Task _ (Event.Insertion BankAccountEvent)
newInsertion index = do
  id <- Uuid.generate
  newMetadata <- EventMetadata.new
  let event = MoneyDeposited {amount = 10} -- Simple default event for tests
  let localPosition =
        fromIntegral index
          |> Event.StreamPosition
          |> Just
  let metadata =
        newMetadata
          { EventMetadata.localPosition = localPosition,
            EventMetadata.eventId = id
          }
  Task.yield
    Event.Insertion
      { id = id,
        event = event,
        metadata = metadata
      }