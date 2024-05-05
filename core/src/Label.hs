{-# OPTIONS_GHC -Wno-orphans #-}

module Label (
  Label (..),
  Literal,
  toString,
  IsLabel (..),
) where

import Data.Data qualified as Data
import GHC.Base qualified as GHC
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits qualified as GHCTypeLits
import Operators
import Reflect qualified
import Text


-- | The `Label` type is a type that represents a type-safe text. It can be used to define
-- fields in records, or to define properties in a schema. It is a type-safe way to define
-- strings that are used as keys in records or schemas.
data Label (name :: GHC.Symbol) = Label
  deriving (Reflect.Shape, Reflect.TypeInfo)


-- | The `LabelLiteral` trait defines that a label is known at a compile-time. This is used
-- to ensure that the label is a type-safe text.
type Literal name = GHCTypeLits.KnownSymbol name


instance forall name. (Literal name) => IsLabel name (Label name) where
  fromLabel = Label


-- | Convert a `Label` to a `Text`.
toString :: forall name. (Literal name) => Label name -> Text
toString _ =
  Data.Proxy @name
    |> GHCTypeLits.symbolVal
    |> Convert.fromLegacy