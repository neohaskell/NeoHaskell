module Shop.Cart.Events.ItemAdded (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Eq)

deriveEvent ''Event
