{-| Define a testbed entity with protected fields retained across event reconstruction.
Use 'ProtectedCreated' to populate its fields.
-}
module Testbed.Erasable.Core (
  ProtectedEntity (..),
  ProtectedEvent (..),
  ProtectedCreated (..),
  SecretPayload (..),
) where

import Core
import Erasable (Erasable)
import Json qualified
import Schema qualified
import Service.Entity.Core qualified as Entity
import Uuid qualified


data SecretPayload = SecretPayload
  { name :: Text,
    aliases :: [Maybe Text]
  }
  deriving (Generic)


instance Json.ToJSON SecretPayload
instance Json.FromJSON SecretPayload
instance Schema.ToSchema SecretPayload


data ProtectedEntity = ProtectedEntity
  { protectedId :: Uuid,
    firstSecret :: Maybe (Erasable SecretPayload),
    secondSecret :: Maybe (Erasable SecretPayload),
    mappedSecret :: Maybe (Erasable SecretPayload)
  }
  deriving (Generic)


instance Json.ToJSON ProtectedEntity
instance Json.FromJSON ProtectedEntity


type instance NameOf ProtectedEntity = "ProtectedEntity"


data ProtectedCreated = ProtectedCreated
  { createdId :: Uuid,
    createdFirst :: Erasable SecretPayload,
    createdSecond :: Erasable SecretPayload,
    createdMapped :: Erasable SecretPayload
  }


deriveEvent ''ProtectedCreated


type instance EntityOf ProtectedCreated = ProtectedEntity


deriveEntityEvents ''ProtectedEntity [''ProtectedCreated]


instance Entity ProtectedEntity where
  initialStateImpl = ProtectedEntity
    { protectedId = Uuid.nil,
      firstSecret = Nothing,
      secondSecret = Nothing,
      mappedSecret = Nothing
    }
  updateImpl event entity = case event of
    ProtectedEventProtectedCreated created ->
      entity
        { protectedId = created.createdId,
          firstSecret = Just created.createdFirst,
          secondSecret = Just created.createdSecond,
          mappedSecret = Just created.createdMapped
        }


instance Entity.Event ProtectedCreated where
  getEventEntityIdImpl created = created.createdId
