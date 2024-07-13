-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
) where

import Appendable as Reexported ((++))
import Basics as Reexported
import Char as Reexported (Char)
import Command as Reexported (Command)
import ConcurrentVar as Reexported (ConcurrentVar)
import Console as Reexported (print, readLine)
import Default as Reexported (Default (..), defaultValue)
import Function as Reexported
import LinkedList as Reexported (LinkedList)
import Map as Reexported (Map)
import Maybe as Reexported (Maybe (..))
import Path as Reexported (Absolute, Directory, File, Path, RelativeTo)
import Platform as Reexported (Platform)
import Result as Reexported (Result)
import Text as Reexported (Text)
import ToText as Reexported (ToText, toText)
import Unknown as Reexported (Unknown)
import Var as Reexported (Var)
import Version as Reexported (Version, version)
