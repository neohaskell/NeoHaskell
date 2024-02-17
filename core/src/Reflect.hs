{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflect (Shape, TypeInfo, typeNameOf, inspectTypeName, inspectPackageName, inspectModuleName, packageNameOf, moduleNameOf) where

import Data.Data qualified as Data
import Debug
import GHC.Generics qualified as Generics
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.Syntax
import Operators
import Traits.Addable
import Types


type Shape = Data.Data


type TypeInfo = Data.Typeable


typeNameOf :: TypeInfo value => value -> String
typeNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConName
    |> Convert.fromLegacy


moduleNameOf :: TypeInfo value => value -> String
moduleNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConModule
    |> Convert.fromLegacy


packageNameOf :: TypeInfo value => value -> String
packageNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConPackage
    |> Convert.fromLegacy


inspectTypeName :: forall value. (TypeInfo value) => String
inspectTypeName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConName
    |> Convert.fromLegacy


inspectModuleName :: forall value. (TypeInfo value) => String
inspectModuleName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConModule
    |> Convert.fromLegacy


inspectPackageName :: forall value. (TypeInfo value) => String
inspectPackageName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConPackage
    |> Convert.fromLegacy


type NoArgumentsConstructor = Generics.U1


type TupleType first rest = first Generics.:*: rest


type DataTypeInfo = Generics.D1


type ConstructorInfo = Generics.C1


type FieldInfo = Generics.S1


type RecordValue = Generics.Rec0


class Selectors rep where
  selectors :: Array (String, String)


instance Selectors rest => Selectors (DataTypeInfo ignored rest) where
  selectors = selectors @rest


instance Selectors rest => Selectors (ConstructorInfo ignored rest) where
  selectors = selectors @rest


instance (Generics.Selector fieldInfo, TypeInfo typeValue) => Selectors (FieldInfo fieldInfo (RecordValue typeValue)) where
  selectors =
    [(Generics.selName (todo :: FieldInfo fieldInfo (RecordValue typeValue) ()) |> Convert.fromLegacy, inspectTypeName @typeValue)]


instance (Selectors first, Selectors rest) => Selectors (TupleType first rest) where
  selectors = selectors @first + selectors @rest


instance Selectors NoArgumentsConstructor where
  selectors = []