module Integration.App where

import Core


data CreateCart = CreateCart
  deriving (Generic)


instance Command CreateCart
