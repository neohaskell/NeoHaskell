module Traits.Serializable (
  Serializable (..),
  Format (..),
) where

import Meta (TypeString)
import Types


data Format (format :: TypeString) = Format


class Serializable (format :: TypeString) (from :: Type) (to :: Type) where
  serialize :: Format format -> from -> to
  deserialize :: Format format -> to -> Result from String
