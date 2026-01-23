module Testbed.Document.Core (
  DocumentEntity (..),
  DocumentEvent (..),
  initialState,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Service.FileUpload.Core (FileRef)
import Uuid qualified


data DocumentEntity = DocumentEntity
  { documentId :: Uuid
  , title :: Text
  , attachmentRef :: Maybe FileRef
  , createdAt :: Maybe Int64
  }
  deriving (Generic)


instance Json.FromJSON DocumentEntity


instance Json.ToJSON DocumentEntity


initialState :: DocumentEntity
initialState =
  DocumentEntity
    { documentId = Uuid.nil
    , title = ""
    , attachmentRef = Nothing
    , createdAt = Nothing
    }


type instance NameOf DocumentEntity = "DocumentEntity"


instance Entity DocumentEntity where
  initialStateImpl = initialState
  updateImpl = update


data DocumentEvent
  = DocumentCreated
      { entityId :: Uuid
      , title :: Text
      , attachmentRef :: FileRef
      , createdAt :: Int64
      }
  deriving (Generic)


getEventEntityId :: DocumentEvent -> Uuid
getEventEntityId event = case event of
  DocumentCreated {entityId} -> entityId


type instance EventOf DocumentEntity = DocumentEvent


type instance EntityOf DocumentEvent = DocumentEntity


instance Event DocumentEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON DocumentEvent


instance Json.ToJSON DocumentEvent


update :: DocumentEvent -> DocumentEntity -> DocumentEntity
update event _entity = case event of
  DocumentCreated {entityId, title, attachmentRef, createdAt} ->
    DocumentEntity
      { documentId = entityId
      , title = title
      , attachmentRef = Just attachmentRef
      , createdAt = Just createdAt
      }
