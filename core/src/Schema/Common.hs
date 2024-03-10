module Schema.Common (
  Schema (..),
  andThen,
  (>>=),
  defines,
  text,
  PropertyOptions (..),
  bool,
  int,
  field,
) where

import Bool
import Debug.ToDo (todo)
import Int
import Meta (TypeReference)
import Schema.Types
import String
import Traits.Defaultable


text :: PropertyOptions String
text = defaultValue


bool :: PropertyOptions Bool
bool = defaultValue


int :: PropertyOptions Int
int = defaultValue


field :: forall a. (Defaultable a) => TypeReference a -> PropertyOptions a
field _ = defaultValue


andThen :: (input -> Schema output) -> Schema input -> Schema output
andThen = todo


defines :: someType -> Schema someType
defines = todo


(>>=) :: (SchemaOperation f input) => f input -> (input -> Schema output) -> Schema output
(>>=) a b = andThen b (convert a)