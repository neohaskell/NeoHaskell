module Int (
  module Reexported,
  toString,
) where

import Data.Text qualified as Text
import GHC.Types as Reexported (Int)
import Pipe
import String (String (INTERNAL_CORE_STRING_CONSTRUCTOR))
import Text.Show qualified as GHC


toString :: Int -> String
toString number =
  (GHC.show number)
    |> Text.pack
    |> INTERNAL_CORE_STRING_CONSTRUCTOR
{-# INLINE toString #-}