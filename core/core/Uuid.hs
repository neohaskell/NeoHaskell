module Uuid (
  Uuid,
  generate,
  generateV5,
  toLegacy,
  fromLegacy,
  toText,
  fromText,
  nil,
) where

import Basics
import Bytes qualified
import Data.Default (Default (..))
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as V4
import Data.UUID.V5 qualified as V5 -- HOOK-ALLOW: this module IS the Core wrapper for UUID; V5 is the sibling of the already-wrapped V4
import Json (FromJSON, ToJSON)
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


newtype Uuid = Uuid (UUID.UUID)
  deriving (Eq, Ord, Generic)


instance ToJSON Uuid


instance FromJSON Uuid


instance Show Uuid where
  show (Uuid uuid) = show uuid


instance Default Uuid where
  def = UUID.nil |> Uuid


generate :: Task _ Uuid
generate = do
  uuid <- Task.fromIO V4.nextRandom
  Task.yield (Uuid uuid)


toLegacy :: Uuid -> UUID.UUID
toLegacy (Uuid uuid) = uuid


fromLegacy :: UUID.UUID -> Uuid
fromLegacy uuid = Uuid uuid


toText :: Uuid -> Text
toText (Uuid uuid) = do
  UUID.toText uuid


nil :: Uuid
nil = fromLegacy UUID.nil


-- | Derive a deterministic UUID (version 5, RFC 4122) from a namespace and a
-- name. The same namespace and name always produce the same UUID, on every
-- machine and in every process.
--
-- Use it when an identifier should be a function of a natural key — deriving
-- an entity id from an external system's string id, so that the same key
-- always resolves to the same entity:
--
-- @
-- getEntityId :: RegisterProject -> Maybe Uuid
-- getEntityId command =
--   Uuid.generateV5 projectNamespace command.repoPath |> Just
-- @
--
-- The namespace is explicit on purpose: it is the collision domain, and
-- deriving two unrelated things from the same name under one namespace gives
-- them the same id. Build one with 'fromText'.
--
-- SECURITY: the result is __not a secret__. Anyone who knows the namespace and
-- the name reproduces it exactly, and names from a small space can be guessed.
-- Never use it for session ids, capability tokens, reset links, or anything
-- else that must be unguessable — use 'generate' (random) for those.
--
-- >>> Uuid.fromText "6ba7b810-9dad-11d1-80b4-00c04fd430c8" |> Maybe.getOrDie |> (\ns -> Uuid.generateV5 ns "python.org") |> Uuid.toText
-- "886313e1-3b8a-5372-9b90-0c9aee199e5d"
generateV5 :: Uuid -> Text -> Uuid
generateV5 (Uuid namespace) name =
  name
    |> Text.toBytes
    |> Bytes.unpack
    |> V5.generateNamed namespace
    |> Uuid


-- | Parse a UUID from its text representation.
-- Returns Nothing if the text is not a valid UUID.
fromText :: Text -> Maybe Uuid
fromText text =
  case UUID.fromText text of
    Nothing -> Nothing
    Just uuid -> Just (Uuid uuid)
