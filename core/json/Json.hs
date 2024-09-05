module Json (
  Decodable,
  Encodable,
  Aeson.Value,
  Aeson.FromJSON,
  Aeson.ToJSON,
  decodeText,
  encode,
) where

import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Either qualified as Either
import Data.Text.Lazy qualified as Data.Text
import Result (Result)
import Result qualified
import Text (Text)
import Text qualified


type Decodable value = Aeson.FromJSON value


type Encodable value = Aeson.ToJSON value


decodeText :: (Decodable value) => Text -> Result Text value
decodeText text = case Aeson.eitherDecodeStrictText text of
  Either.Left error -> Result.Err (Text.fromLinkedList error)
  Either.Right value -> Result.Ok value


encode :: (Encodable value) => value -> Text
encode value =
  AesonText.encodeToLazyText value
    |> Data.Text.toStrict