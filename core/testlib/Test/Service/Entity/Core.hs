module Test.Service.Entity.Core (
  BankAccountEvent (..),
  BankAccountState (..),
  initialState,
) where

import Core
import Json qualified


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
