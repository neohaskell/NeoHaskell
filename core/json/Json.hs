module Json (
  Aeson.Value,
  Aeson.FromJSON,
  Aeson.FromJSONKey,
  Aeson.ToJSON,
  decodeText,
  encode,
  decode,
  encodeText,
  decodeBytes,
  null,
) where

import Array (Array)
import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Either qualified as Either
import Data.Text.Lazy qualified as Data.Text
import Default (Default (..))
import Result (Result)
import Result qualified
import Text (Text)
import Text qualified


decodeText :: (Aeson.FromJSON value) => Text -> Result Text value
decodeText text = do
  let bs = Text.convert text
  case Aeson.eitherDecodeStrict bs of
    Either.Left error -> Result.Err (Text.fromLinkedList error)
    Either.Right value -> Result.Ok value


decodeBytes :: (Aeson.FromJSON value) => Bytes -> Result Text value
decodeBytes bytes = do
  let bs = bytes |> Bytes.unwrap
  case Aeson.eitherDecodeStrict bs of
    Either.Left error -> Result.Err (Text.fromLinkedList error)
    Either.Right value -> Result.Ok value


encodeText :: (Aeson.ToJSON value) => value -> Text
encodeText value =
  AesonText.encodeToLazyText value
    |> Data.Text.toStrict


encode :: (Aeson.ToJSON value) => value -> Aeson.Value
encode = Aeson.toJSON


decode :: (Aeson.FromJSON value) => Aeson.Value -> Result Text value
decode value = case Aeson.fromJSON value of
  Aeson.Error error -> Result.Err (Text.fromLinkedList error)
  Aeson.Success val -> Result.Ok val


instance (Aeson.FromJSON a) => Aeson.FromJSON (Array a)


instance (Aeson.ToJSON a) => Aeson.ToJSON (Array a)


instance Default Aeson.Value where
  def = Aeson.Null


-- | The JSON null value.
--
-- @
-- Json.null  -- represents null in JSON
-- @
null :: Aeson.Value
null = Aeson.Null