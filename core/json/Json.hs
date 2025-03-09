module Json (
  Decodable,
  Encodable,
  Aeson.Value,
  Aeson.FromJSON,
  Aeson.FromJSONKey,
  Aeson.ToJSON,
  decodeText,
  encode,
  decode,
  encodeText,
) where

import Array (Array)
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
decodeText text = do
  let bs = Text.convert text
  case Aeson.eitherDecodeStrict bs of
    Either.Left error -> Result.Err (Text.fromLinkedList error)
    Either.Right value -> Result.Ok value


encodeText :: (Encodable value) => value -> Text
encodeText value =
  AesonText.encodeToLazyText value
    |> Data.Text.toStrict


encode :: (Encodable value) => value -> Aeson.Value
encode = Aeson.toJSON


decode :: (Decodable value) => Aeson.Value -> Result Text value
decode value = case Aeson.fromJSON value of
  Aeson.Error error -> Result.Err (Text.fromLinkedList error)
  Aeson.Success val -> Result.Ok val


instance (Aeson.FromJSON a) => Aeson.FromJSON (Array a)


instance (Aeson.ToJSON a) => Aeson.ToJSON (Array a)