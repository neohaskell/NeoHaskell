module Json (
  Decodable,
  Encodable,
  decodeText,
  encode,
) where

import Core
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Either qualified as Either
import Data.Text.Lazy qualified as Data.Text
import Result qualified
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