{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSON (JSON, format, parseString, encodeString, implementSerializable) where

import Bytes (Bytes)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as AesonTH
import Data.Aeson.Text qualified as Aeson
import Data.Aeson.Types qualified as AesonType
-- TODO: Cleanup Lazy imports into its own data types
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text.Lazy qualified as LazyText
import HaskellCompatibility.Conversion qualified as Convert
import Meta (DeclarationMacro, DeclarationName)
import Operators
import Result qualified
import Traits.Serializable
import Types


newtype JSON = JSON Aeson.Value


parseString :: Serializable "JSON" a String => String -> Result a String
parseString string = deserialize format string


encodeString :: Serializable "JSON" a String => a -> String
encodeString value = serialize format value


format :: Format "JSON"
format = Format


implementSerializable :: DeclarationName -> DeclarationMacro
implementSerializable = do
  AesonTH.deriveJSON Aeson.defaultOptions


-- | Instance to encode any value into a `JSON` object
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


-- | Instance to encode any value into a `String`
instance
  ( Aeson.ToJSON value,
    Aeson.FromJSON value
  ) =>
  Serializable "JSON" value String
  where
  serialize _ value =
    Aeson.encodeToLazyText (Aeson.toJSON value)
      |> LazyText.toStrict
      |> Convert.fromLegacy


  deserialize _ string =
    Convert.toLegacy string
      |> Aeson.eitherDecodeStrictText @value
      |> Convert.fromLegacy
      |> Result.applyToError (Convert.fromLegacy @[Char])


-- | Instance to encode any value into a `Bytes` (ByteString)
instance
  ( Aeson.ToJSON value,
    Aeson.FromJSON value
  ) =>
  Serializable "JSON" value Bytes
  where
  serialize _ value =
    Aeson.encode value
      |> LazyByteString.toStrict
      |> Convert.fromLegacy


  deserialize _ bytes =
    Convert.toLegacy bytes
      |> Aeson.eitherDecodeStrict @value
      |> Convert.fromLegacy
      |> Result.applyToError (Convert.fromLegacy @[Char])