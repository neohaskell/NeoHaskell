module Schema.Types (
  Schema (..),
  PropertyOptions (..),
  map,
  yield,
  apply,
  SchemaOperation (..),
  SchemaType (..),
  foo,
) where

import Control.Applicative qualified as GHC
import HaskellCompatibility.Syntax
import Language.Haskell.TH qualified as TH
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


data SchemaType
  = SchemaText
  | SchemaBool
  | SchemaInt


data PropertyOptions a = PropertyOptions
  { name :: String,
    description :: String,
    shorthand :: Char,
    defaultsTo :: a,
    hidden :: Bool,
    placeholder :: String,
    schemaType :: SchemaType
  }


instance (Defaultable a) => Defaultable (PropertyOptions a) where
  defaultValue =
    PropertyOptions
      { name = "",
        description = "",
        shorthand = ' ',
        defaultsTo = defaultValue,
        hidden = False,
        placeholder = "",
        schemaType = SchemaText
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
      placeholder = options.placeholder,
      schemaType = SchemaText
    }


class SchemaOperation f a where
  convert :: f a -> Schema a


instance (Defaultable a) => SchemaOperation PropertyOptions a where
  convert = PropertySchema


instance SchemaOperation Schema a where
  convert x = x


foo :: TH.Q (PropertyOptions a -> TH.Exp)
foo = GHC.pure \options -> case options.schemaType of
  SchemaText -> [e|strOption|]
  SchemaBool -> [e|switch|]
  SchemaInt -> [e|option auto|]
