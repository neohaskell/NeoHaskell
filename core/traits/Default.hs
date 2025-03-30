{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Default (Default (..), defaultValue) where

import Char (Char)
import Data.Default
import Text (Text)


defaultValue :: (Default value) => value
defaultValue = def


instance Default Char where
  def = '\0'


instance Default Text where
  def = ""
