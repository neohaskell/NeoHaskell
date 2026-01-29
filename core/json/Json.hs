module Json (
  Aeson.Value,
  Aeson.FromJSON (..),
  Aeson.FromJSONKey,
  Aeson.ToJSON (..),
  Aeson.Object,
  decodeText,
  encode,
  decode,
  encodeText,
  decodeBytes,
  null,
  -- * Parsing helpers
  withText,
  withObject,
  (.:),
  (.:?),
  (.!=),
  yield,
  fail,
  -- * Encoding helpers
  object,
  (.=),
) where

import Array (Array)
import Basics
import Bytes (Bytes)
import Bytes qualified
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.Text qualified as AesonText
import Data.Aeson.Types qualified as AesonTypes
import Data.Either qualified as Either
import Data.Text.Lazy qualified as Data.Text
import Default (Default (..))
import Maybe (Maybe)
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


-- | Parse a JSON text value.
--
-- @
-- instance Json.FromJSON Role where
--   parseJSON = Json.withText "Role" \\text ->
--     case text of
--       "admin" -> Json.yield Admin
--       other -> Json.fail [fmt|Unknown role: #{other}|]
-- @
withText :: Text -> (Text -> AesonTypes.Parser value) -> Aeson.Value -> AesonTypes.Parser value
withText label f = AesonTypes.withText (Text.toLinkedList label) f


-- | Parse a JSON object.
--
-- @
-- instance Json.FromJSON User where
--   parseJSON = Json.withObject "User" \\obj -> do
--     name <- obj Json..: "name"
--     age <- obj Json..: "age"
--     Json.yield User {name, age}
-- @
withObject :: Text -> (Aeson.Object -> AesonTypes.Parser value) -> Aeson.Value -> AesonTypes.Parser value
withObject label f = AesonTypes.withObject (Text.toLinkedList label) f


-- | Extract a required field from a JSON object.
(.:) :: (Aeson.FromJSON value) => Aeson.Object -> Text -> AesonTypes.Parser value
(.:) obj key = obj Aeson..: AesonKey.fromText key


-- | Extract an optional field from a JSON object.
(.:?) :: (Aeson.FromJSON value) => Aeson.Object -> Text -> AesonTypes.Parser (Maybe value)
(.:?) obj key = obj Aeson..:? AesonKey.fromText key


-- | Provide a default value for an optional field.
(.!=) :: AesonTypes.Parser (Maybe value) -> value -> AesonTypes.Parser value
(.!=) = (Aeson..!=)


-- | Return a successfully parsed value.
--
-- @
-- Json.yield User {name, age}
-- @
yield :: value -> AesonTypes.Parser value
yield = Monad.return


-- | Fail parsing with an error message.
--
-- @
-- Json.fail "Invalid format"
-- @
fail :: Text -> AesonTypes.Parser value
fail msg = Monad.fail (Text.toLinkedList msg)


-- | Create a JSON object from key-value pairs.
--
-- @
-- instance Json.ToJSON User where
--   toJSON user = Json.object
--     [ ("name", Json.toJSON user.name)
--     , ("age", Json.toJSON user.age)
--     ]
-- @
object :: [(Text, Aeson.Value)] -> Aeson.Value
object pairs = Aeson.object [(AesonKey.fromText k, v) | (k, v) <- pairs]


-- | Create a key-value pair for JSON objects.
--
-- @
-- Json.object
--   [ "name" Json..= user.name
--   , "age" Json..= user.age
--   ]
-- @
(.=) :: (Aeson.ToJSON value) => Text -> value -> (Text, Aeson.Value)
key .= val = (key, Aeson.toJSON val)