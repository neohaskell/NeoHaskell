module Traits.Serializable (
  Serializable (..),
) where

import Types


class Serializable (format :: Type) (value :: Type) where
  genericSerialize :: value -> format
  genericDeserialize :: format -> Result value String
