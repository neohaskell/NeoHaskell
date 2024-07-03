-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
) where

import Appendable as Reexported ((++))
import Applicable as Reexported (pure)
import Basics as Reexported
import Char as Reexported (Char)
import Console as Reexported (print, readLine)
import Default as Reexported (Default, defaultValue)
import Function as Reexported
import LinkedList as Reexported (LinkedList)
import Maybe as Reexported (Maybe (..))
import Result as Reexported (Result)
import Text as Reexported (Text)
import ToText as Reexported (ToText, toText)
import Version as Reexported (Version, version)
