module Test.Service.EntityFetcher.Core (
  BankAccountEvent (..),
  BankAccountState (..),
  initialState,
  applyEvent,
  newFetcher,
) where

import Core
import Json qualified
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.EventStore.Core (EventStore)
import Test.Service.EventStore.Core (BankAccountEvent (..))


-- | Example entity state for a bank account
data BankAccountState = BankAccountState
  { balance :: Int,
    isOpen :: Bool,
    version :: Int
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON BankAccountState


instance Json.FromJSON BankAccountState


-- | Initial state for a new bank account
initialState :: BankAccountState
initialState =
  BankAccountState
    { balance = 0,
      isOpen = False,
      version = 0
    }


-- | Apply a bank account event to the current state
applyEvent :: BankAccountEvent -> BankAccountState -> BankAccountState
applyEvent event state = do
  let newVersion = state.version + 1
  case event of
    AccountOpened {initialBalance} ->
      BankAccountState
        { balance = initialBalance,
          isOpen = True,
          version = newVersion
        }
    MoneyDeposited {amount} ->
      state
        { balance = state.balance + amount,
          version = newVersion
        }
    MoneyWithdrawn {amount} ->
      state
        { balance = state.balance - amount,
          version = newVersion
        }
    AccountClosed ->
      state
        { isOpen = False,
          version = newVersion
        }


-- | Create a new entity fetcher for bank accounts
newFetcher :: EventStore BankAccountEvent -> Task EntityFetcher.Error (EntityFetcher BankAccountState BankAccountEvent)
newFetcher eventStore = do
  EntityFetcher.new eventStore initialState applyEvent
