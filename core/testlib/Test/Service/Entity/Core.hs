module Test.Service.Entity.Core (
  BankAccountEvent (..),
  BankAccountState (..),
  initialState,
  applyEvent,
  newReducer,
) where

import Core
import Json qualified
import Service.Entity.Core (EntityReducer)
import Service.Entity.Core qualified as Entity
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
applyEvent :: BankAccountState -> BankAccountEvent -> BankAccountState
applyEvent state event = do
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


-- | Create a new entity reducer for bank accounts
newReducer :: EventStore BankAccountEvent -> Task Entity.Error (EntityReducer BankAccountState BankAccountEvent)
newReducer eventStore = do
  Entity.new eventStore initialState applyEvent
