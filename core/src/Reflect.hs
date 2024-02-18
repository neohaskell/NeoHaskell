{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflect (Shape, TypeInfo, typeNameOf, inspectTypeName, inspectPackageName, inspectModuleName, packageNameOf, moduleNameOf) where

import Data.Data qualified as Data
import HaskellCompatibility.Conversion qualified as Convert
import Operators
import String


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
