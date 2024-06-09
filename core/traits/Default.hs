{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Default (Default (..), defaultValue) where

import Data.Default
import Data.Monoid qualified as Monoid


defaultValue :: (Default value) => value
defaultValue = def


instance (Monoid.Monoid value) => Default value where
  def = Monoid.mempty