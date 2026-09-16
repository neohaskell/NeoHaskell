module Shop.Cart.Events.CartCreated (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , ownerId :: Text
  }
  deriving (Eq)

deriveEvent ''Event
