{-| Define the protected value envelope passed between decisions and storage.
Application code uses 'Erasable' instead of constructing this representation.
-}
module Erasable.Internal (
  ProtectedValue (..),
  NewKey (..),
  KeyState (..),
  references,
  renderWith,
  resealWith,
  validateStorageContext,
  contextBytes,
) where

import Array (Array)
import Array qualified
import Basics
import Erasable.Cipher (Envelope, Key)
import Erasable.Cipher qualified as Cipher
import Json qualified
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


data KeyState = Active Key | Erased | Missing
  deriving (Show)


references :: Json.Value -> Result Text (Array Text)
references jsonValue =
  case Json.parts jsonValue of
    Json.ObjectParts pairs ->
      if hasErasableMarker pairs
        then do
          protected <- Json.decode @ProtectedValue jsonValue
          Ok (Array.wrap protected.reference)
        else collect (pairs |> Array.map (\(_, value) -> value))
    Json.ArrayParts values -> collect values
    Json.Scalar _ -> Ok Array.empty


hasErasableMarker :: Array (Text, Json.Value) -> Bool
hasErasableMarker pairs = pairs |> Array.any (\(key, _) -> key == "$erasable")


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
  (Text -> Task Text KeyState) ->
  Json.Value ->
  Task Text Json.Value
renderWith lookupKey jsonValue =
  case Json.parts jsonValue of
    Json.ObjectParts pairs ->
      if hasErasableMarker pairs
        then renderProtected lookupKey jsonValue
        else do
          rendered <- pairs |> Task.mapArray (\(key, value) -> do
            result <- renderWith lookupKey value
            Task.yield (key, result))
          Json.object (Array.toLinkedList rendered) |> Task.yield
    Json.ArrayParts values -> do
      rendered <- values |> Task.mapArray (renderWith lookupKey)
      Json.array rendered |> Task.yield
    Json.Scalar _ -> Task.yield jsonValue


resealWith ::
  (Text -> Task Text KeyState) ->
  Text ->
  Json.Value ->
  Task Text Json.Value
resealWith lookupKey base jsonValue = resealAt lookupKey base "" jsonValue


resealAt ::
  (Text -> Task Text KeyState) ->
  Text ->
  Text ->
  Json.Value ->
  Task Text Json.Value
resealAt lookupKey base path jsonValue =
  case Json.parts jsonValue of
    Json.ObjectParts pairs ->
      if hasErasableMarker pairs
        then do
          protected <-
            case Json.decode @ProtectedValue jsonValue of
              Err _ -> Task.throw "Malformed erasable envelope"
              Ok value -> Task.yield value
          keyState <- lookupKey protected.reference
          case keyState of
            Missing -> Task.throw "Erasable key record is missing"
            Erased -> do
              let context = [fmt|#{base}#{path}|]
              Task.yield (Json.toJSON (protected {context}))
            Active key -> do
              plaintext <-
                case Cipher.open key (contextBytes protected.reference protected.context) protected.envelope of
                  Err err -> Task.throw err
                  Ok value -> Task.yield value
              let context = [fmt|#{base}#{path}|]
              envelope <- Cipher.seal key (contextBytes protected.reference context) plaintext
              Task.yield (Json.toJSON (protected {context, envelope}))
        else do
          resealed <- pairs |> Task.mapArray (\(key, value) -> do
            result <- resealAt lookupKey base (objectPath path key) value
            Task.yield (key, result))
          Json.object (Array.toLinkedList resealed) |> Task.yield
    Json.ArrayParts values -> do
      resealed <- values |> Array.indexed |> Task.mapArray (\(index, value) -> resealAt lookupKey base (arrayPath path index) value)
      Json.array resealed |> Task.yield
    Json.Scalar _ -> Task.yield jsonValue


validateStorageContext :: Text -> Json.Value -> Result Text Unit
validateStorageContext base jsonValue = validateAt base "" jsonValue


validateAt :: Text -> Text -> Json.Value -> Result Text Unit
validateAt base path jsonValue =
  case Json.parts jsonValue of
    Json.ObjectParts pairs ->
      if hasErasableMarker pairs
        then do
          protected <- Json.decode @ProtectedValue jsonValue
          if protected.context == [fmt|#{base}#{path}|]
            then Ok unit
            else Err "Erasable storage context does not match its location"
        else pairs |> Array.foldl
          (\(key, value) accumulated -> do
            accumulated
            validateAt base (objectPath path key) value)
          (Ok unit)
    Json.ArrayParts values -> values |> Array.indexed |> Array.foldl
      (\(index, value) accumulated -> do
        accumulated
        validateAt base (arrayPath path index) value)
      (Ok unit)
    Json.Scalar _ -> Ok unit


objectPath :: Text -> Text -> Text
objectPath path key = [fmt|#{path}/o:#{escapeObjectKey key}|]


escapeObjectKey :: Text -> Text
escapeObjectKey key = key |> Text.replace "~" "~0" |> Text.replace "/" "~1"


arrayPath :: Text -> Int -> Text
arrayPath path index = [fmt|#{path}/a:#{index}|]


contextBytes :: Text -> Text -> Bytes.Bytes
contextBytes reference context = Text.toBytes [fmt|#{reference}:#{context}|]


renderProtected ::
  (Text -> Task Text KeyState) ->
  Json.Value ->
  Task Text Json.Value
renderProtected lookupKey jsonValue = do
  protected <-
    case Json.decode @ProtectedValue jsonValue of
      Err _ -> Task.throw "Malformed erasable envelope"
      Ok value -> Task.yield value
  state <- lookupKey protected.reference
  case state of
    Missing -> Task.throw "Erasable key record is missing"
    Erased -> Task.yield (publicValue "Erased" Json.null)
    Active key -> do
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
