{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflect (Shape, TypeInfo, typeNameOf, inspectTypeName, inspectPackageName, inspectModuleName, packageNameOf, moduleNameOf) where

import Data.Data qualified as Data
import HaskellCompatibility.Conversion qualified as Convert
import Operators
import Text


type Shape = Data.Data


type TypeInfo = Data.Typeable


typeNameOf :: TypeInfo value => value -> Text
typeNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConName
    |> Convert.fromLegacy


moduleNameOf :: TypeInfo value => value -> Text
moduleNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConModule
    |> Convert.fromLegacy


packageNameOf :: TypeInfo value => value -> Text
packageNameOf value =
  Data.typeOf value
    |> Data.typeRepTyCon
    |> Data.tyConPackage
    |> Convert.fromLegacy


inspectTypeName :: forall value. (TypeInfo value) => Text
inspectTypeName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConName
    |> Convert.fromLegacy


inspectModuleName :: forall value. (TypeInfo value) => Text
inspectModuleName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConModule
    |> Convert.fromLegacy


inspectPackageName :: forall value. (TypeInfo value) => Text
inspectPackageName =
  Data.typeRep (Data.Proxy @value)
    |> Data.typeRepTyCon
    |> Data.tyConPackage
    |> Convert.fromLegacy
