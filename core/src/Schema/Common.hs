module Schema.Common (
  Schema (..),
  andThen,
  (>>=),
  defines,
  text,
  PropertyOptions (..),
  bool,
  int,
) where

import Bool
import Debug.ToDo (todo)
import Int
import Schema.Types
import String
import Traits.Defaultable


text :: PropertyOptions String
text = defaultValue


bool :: PropertyOptions Bool
bool = defaultValue


int :: PropertyOptions Int
int = defaultValue


andThen :: (input -> Schema output) -> Schema input -> Schema output
andThen = todo


defines :: someType -> Schema someType
defines = todo


(>>=) :: (SchemaOperation f input) => f input -> (input -> Schema output) -> Schema output
(>>=) a b = andThen b (convert a)