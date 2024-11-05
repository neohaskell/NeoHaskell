module Toml (
  Decodable,
  Encodable,
  TomlS.FromValue,
  TomlS.ToValue,
  Error,
  decodeText,
  encodeText,
) where

import Array qualified
import Basics
import Result (Result (..))
import Text (Text)
import Text qualified
import ToText (toPrettyText)
import "toml-parser" Toml qualified as TomlP
import "toml-parser" Toml.Schema qualified as TomlS


type Decodable value = TomlS.FromValue value


type Encodable value = TomlS.ToTable value


type Error = TomlP.DecodeError


decodeText :: (Decodable value) => Text -> Result Text value
decodeText text = case TomlP.decode text of
  TomlP.Failure errors ->
    Array.fromLinkedList errors
      |> Array.map Text.fromLinkedList
      |> Text.joinWith "\n"
      |> Err
  TomlP.Success _ value -> Ok value


encodeText :: (Encodable value) => value -> Text
encodeText value =
  TomlP.encode value
    |> toPrettyText