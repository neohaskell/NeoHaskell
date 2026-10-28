{-| Define the protected value envelope passed between decisions and storage.
Application code uses 'Erasable' instead of constructing this representation.
-}
module Erasable.Internal (
  ProtectedValue (..),
  NewKey (..),
  references,
  renderWith,
  resealWith,
  validateStorageContext,
  contextBytes,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as ByteString
import Data.Vector qualified as Vector
import Erasable.Cipher (Envelope, Key)
import Erasable.Cipher qualified as Cipher
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Bytes qualified


data ProtectedValue = ProtectedValue
  { reference :: Text,
    context :: Text,
    envelope :: Envelope
  }


instance Show ProtectedValue where
  show _ = "Erasable <REDACTED>"


instance Json.ToJSON ProtectedValue where
  toJSON value =
    Json.object
      [ ("$erasable", Json.toJSON (1 :: Int)),
        ("reference", Json.toJSON value.reference),
        ("context", Json.toJSON value.context),
        ("envelope", Json.toJSON value.envelope)
      ]


instance Json.FromJSON ProtectedValue where
  parseJSON = Json.withObject "Erasable" \object -> do
    marker <- object Json..: "$erasable"
    if (marker :: Int) != 1
      then Json.fail "Unsupported erasable value version"
      else do
        reference <- object Json..: "reference"
        context <- object Json..: "context"
        envelope <- object Json..: "envelope"
        Json.yield ProtectedValue {reference, context, envelope}


data NewKey = NewKey
  { reference :: Text,
    key :: Key
  }


instance Show NewKey where
  show _ = "NewErasableKey <REDACTED>"


references :: Json.Value -> Result Text (Array Text)
references jsonValue =
  case jsonValue of
    Aeson.Object object ->
      case KeyMap.lookup "$erasable" object of
        Just _ -> do
          protected <- Json.decode @ProtectedValue jsonValue
          Ok (Array.wrap protected.reference)
        Nothing -> collect (KeyMap.elems object |> Array.fromLinkedList)
    Aeson.Array values -> collect (Vector.toList values |> Array.fromLinkedList)
    _ -> Ok Array.empty


collect :: Array Json.Value -> Result Text (Array Text)
collect values =
  values
    |> Array.foldl
      (\value accumulated -> do
        before <- accumulated
        next <- references value
        Ok (before |> Array.append next))
      (Ok Array.empty)


renderWith ::
  (Text -> Task Text (Maybe (Maybe ByteString.ByteString))) ->
  Json.Value ->
  Task Text Json.Value
renderWith lookupKey jsonValue =
  case jsonValue of
    Aeson.Object object ->
      case KeyMap.lookup "$erasable" object of
        Just _ -> renderProtected lookupKey jsonValue
        Nothing -> do
          let pairs = KeyMap.toList object |> Array.fromLinkedList
          rendered <- pairs |> Task.mapArray (\(key, value) -> do
            result <- renderWith lookupKey value
            Task.yield (key, result))
          KeyMap.fromList (Array.toLinkedList rendered) |> Aeson.Object |> Task.yield
    Aeson.Array values -> do
      rendered <- values |> Vector.toList |> Array.fromLinkedList |> Task.mapArray (renderWith lookupKey)
      Array.toLinkedList rendered |> Vector.fromList |> Aeson.Array |> Task.yield
    _ -> Task.yield jsonValue


resealWith ::
  (Text -> Task Text (Maybe (Maybe Key))) ->
  Text ->
  Json.Value ->
  Task Text Json.Value
resealWith lookupKey base = go ""
 where
  go :: Text -> Json.Value -> Task Text Json.Value
  go path jsonValue = case jsonValue of
    Aeson.Object object ->
      case KeyMap.lookup "$erasable" object of
        Just _ -> do
          protected <-
            case Json.decode @ProtectedValue jsonValue of
              Err _ -> Task.throw "Malformed erasable envelope"
              Ok value -> Task.yield value
          keyState <- lookupKey protected.reference
          case keyState of
            Nothing -> Task.throw "Erasable key record is missing"
            Just Nothing -> do
              let context = [fmt|#{base}#{path}|]
              Task.yield (Json.toJSON (protected {context}))
            Just (Just key) -> do
              plaintext <-
                case Cipher.open key (contextBytes protected.reference protected.context) protected.envelope of
                  Err err -> Task.throw err
                  Ok value -> Task.yield value
              let context = [fmt|#{base}#{path}|]
              envelope <- Cipher.seal key (contextBytes protected.reference context) plaintext
              Task.yield (Json.toJSON (protected {context, envelope}))
        Nothing -> do
          let pairs = KeyMap.toList object |> Array.fromLinkedList
          resealed <- pairs |> Task.mapArray (\(key, value) -> do
            result <- go [fmt|#{path}/#{Key.toText key}|] value
            Task.yield (key, result))
          KeyMap.fromList (Array.toLinkedList resealed) |> Aeson.Object |> Task.yield
    Aeson.Array values -> do
      resealed <- values |> Vector.toList |> Array.fromLinkedList |> Array.indexed |> Task.mapArray (\(index, value) -> go [fmt|#{path}/#{index}|] value)
      Array.toLinkedList resealed |> Vector.fromList |> Aeson.Array |> Task.yield
    _ -> Task.yield jsonValue


validateStorageContext :: Text -> Json.Value -> Result Text Unit
validateStorageContext base = go ""
 where
  go :: Text -> Json.Value -> Result Text Unit
  go path jsonValue =
    case jsonValue of
      Aeson.Object object ->
        case KeyMap.lookup "$erasable" object of
          Just _ -> do
            protected <- Json.decode @ProtectedValue jsonValue
            if protected.context == [fmt|#{base}#{path}|]
              then Ok unit
              else Err "Erasable storage context does not match its location"
          Nothing -> KeyMap.toList object |> Array.fromLinkedList |> Array.foldl
            (\(key, value) accumulated -> do
              accumulated
              go [fmt|#{path}/#{Key.toText key}|] value)
            (Ok unit)
      Aeson.Array values -> Vector.toList values |> Array.fromLinkedList |> Array.indexed |> Array.foldl
        (\(index, value) accumulated -> do
          accumulated
          go [fmt|#{path}/#{index}|] value)
        (Ok unit)
      _ -> Ok unit


contextBytes :: Text -> Text -> Bytes.Bytes
contextBytes reference context = Text.toBytes [fmt|#{reference}:#{context}|]


renderProtected ::
  (Text -> Task Text (Maybe (Maybe ByteString.ByteString))) ->
  Json.Value ->
  Task Text Json.Value
renderProtected lookupKey jsonValue = do
  protected <-
    case Json.decode @ProtectedValue jsonValue of
      Err _ -> Task.throw "Malformed erasable envelope"
      Ok value -> Task.yield value
  state <- lookupKey protected.reference
  case state of
    Nothing -> Task.throw "Erasable key record is missing"
    Just Nothing -> Task.yield (publicValue "Erased" Json.null)
    Just (Just keyBytes) -> do
      key <-
        case Cipher.keyFromBytes (Bytes.fromLegacy keyBytes) of
          Err err -> Task.throw err
          Ok value -> Task.yield value
      plaintext <-
        case Cipher.open key (contextBytes protected.reference protected.context) protected.envelope of
          Err err -> Task.throw err
          Ok value -> Task.yield value
      decoded <-
        case Json.decodeBytes @Json.Value plaintext of
          Err _ -> Task.throw "Erasable plaintext is not JSON"
          Ok value -> Task.yield value
      Task.yield (publicValue "Available" decoded)


publicValue :: Text -> Json.Value -> Json.Value
publicValue tag contents =
  Json.object [("tag", Json.toJSON tag), ("contents", contents)]
