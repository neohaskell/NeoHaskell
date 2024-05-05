module Traits.Default (Default, defaultValue) where

import Data.Default


defaultValue :: (Default value) => value
defaultValue = def