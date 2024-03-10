module Schema.Types (
  Schema (..),
  PropertyOptions (..),
  map,
  yield,
  apply,
  SchemaOperation (..),
) where

import HaskellCompatibility.Syntax
import Optional qualified
import Pipe
import Record
import Traits.Defaultable
import Types


data Schema someType
  = NullSchema (Optional someType)
  | PropertySchema (PropertyOptions someType)
  | forall input. PartialSchema (Schema (input -> someType)) (Schema input)
  | AlternativeSchema (Schema someType) (Schema someType)
  | forall input. ContinuationSchema (Schema input) (input -> Schema someType)


map :: (input -> output) -> Schema input -> Schema output
map callback schema = case schema of
  NullSchema optional -> NullSchema (Optional.applyToContents callback optional)
  PropertySchema options -> PropertySchema (mapDefault callback options)
  PartialSchema f innerSchema -> PartialSchema (map (callback <.) f) innerSchema
  AlternativeSchema left right -> AlternativeSchema (map callback left) (map callback right)
  ContinuationSchema innerSchema f -> ContinuationSchema innerSchema (f .> map callback)


yield :: someType -> Schema someType
yield value = NullSchema (Optional.Some value)


apply :: Schema (input -> output) -> Schema input -> Schema output
apply partialSchema inputSchema = PartialSchema partialSchema inputSchema


-- We could go ahead and define PropertyOptions like this
--
data PropertyOptions a = PropertyOptions
  { name :: String,
    description :: String,
    shorthand :: Char,
    defaultsTo :: a,
    hidden :: Bool,
    placeholder :: String
  }


instance (Defaultable a) => Defaultable (PropertyOptions a) where
  defaultValue =
    PropertyOptions
      { name = "",
        description = "",
        shorthand = ' ',
        defaultsTo = defaultValue,
        hidden = False,
        placeholder = ""
      }


mapDefault :: (input -> output) -> PropertyOptions input -> PropertyOptions output
mapDefault callback options = do
  let newDefaultsTo = callback (options.defaultsTo)
  PropertyOptions
    { name = options.name,
      description = options.description,
      shorthand = options.shorthand,
      defaultsTo = newDefaultsTo,
      hidden = options.hidden,
      placeholder = options.placeholder
    }


class SchemaOperation f a where
  convert :: f a -> Schema a


instance (Defaultable a) => SchemaOperation PropertyOptions a where
  convert = PropertySchema


instance SchemaOperation Schema a where
  convert x = x
