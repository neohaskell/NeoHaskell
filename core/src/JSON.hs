{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module JSON where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as AesonTH
import Data.Aeson.Text qualified as Aeson
import Data.Aeson.Types qualified as AesonType
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as LazyText
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.Generic
import Language.Haskell.TH.Syntax qualified as TH
import Meta (CodeGeneration)
import Operators
import Result qualified
import Traits.Serializable
import Types


newtype JSON = JSON Aeson.Value


format :: Format "JSON"
format = Format


implementSerializable :: DeclarationName -> CodeGeneration [TH.Dec]
implementSerializable = do
  AesonTH.deriveJSON Aeson.defaultOptions


instance
  ( Aeson.ToJSON value,
    Aeson.FromJSON value
  ) =>
  Serializable "JSON" value JSON
  where
  serialize _ value =
    Aeson.toJSON value
      |> JSON


  deserialize _ (JSON json) = do
    let result = Aeson.fromJSON @value json
    case result of
      AesonType.Success decoded ->
        Result.Ok decoded
      AesonType.Error error ->
        Convert.fromLegacy @[Char] @String error
          |> Result.Error