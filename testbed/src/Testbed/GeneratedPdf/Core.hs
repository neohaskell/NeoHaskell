-- | Domain state and events for generated PDFs.
-- The generated PDF commands, query, and outbound worker share this model.
module Testbed.GeneratedPdf.Core (
  GeneratedPdfEntity (..),
  GeneratedPdfEvent (..),
  GeneratedPdfRequestedPayload (..),
  initialState,
) where

import Core
import Integration (FileRef)
import Uuid qualified


data GeneratedPdfEntity = GeneratedPdfEntity
  { documentId :: Uuid.Uuid,
    ownerId :: Text,
    fileRef :: Maybe FileRef,
    requestCount :: Int,
    completedAttempts :: Int
  }


data GeneratedPdfEvent
  = GeneratedPdfRequestRecorded
      { entityId :: Uuid.Uuid,
        requestedOwnerId :: Text,
        generationNumber :: Int
      }
  | GeneratedPdfReadyRecorded
      { entityId :: Uuid.Uuid,
        fileRef :: FileRef,
        generationNumber :: Int
      }


deriveEvent ''GeneratedPdfEvent


data GeneratedPdfRequestedPayload = GeneratedPdfRequestedPayload
  { requestedDocumentId :: Uuid.Uuid,
    requestedOwnerId :: Text,
    requestedGenerationNumber :: Int
  }


deriveEvent ''GeneratedPdfRequestedPayload


type instance EntityOf GeneratedPdfRequestedPayload = GeneratedPdfEntity


initialState :: GeneratedPdfEntity
initialState =
  GeneratedPdfEntity
    { documentId = Uuid.nil,
      ownerId = "",
      fileRef = Nothing,
      requestCount = 0,
      completedAttempts = 0
    }


update :: GeneratedPdfEvent -> GeneratedPdfEntity -> GeneratedPdfEntity
update eventValue entity = case eventValue of
  GeneratedPdfRequestRecorded {entityId, requestedOwnerId, generationNumber} ->
    entity
      { documentId = entityId,
        ownerId = requestedOwnerId,
        requestCount = generationNumber
      }
  GeneratedPdfReadyRecorded {entityId, fileRef, generationNumber} ->
    entity
      { documentId = entityId,
        fileRef = Just fileRef,
        completedAttempts = max entity.completedAttempts generationNumber
      }


getEventEntityId :: GeneratedPdfEvent -> Uuid.Uuid
getEventEntityId eventValue = case eventValue of
  GeneratedPdfRequestRecorded {entityId} -> entityId
  GeneratedPdfReadyRecorded {entityId} -> entityId


deriveEntity ''GeneratedPdfEntity ''GeneratedPdfEvent


instance EventVariantOf GeneratedPdfEvent GeneratedPdfRequestedPayload where
  fromVariant payload =
    GeneratedPdfRequestRecorded
      { entityId = payload.requestedDocumentId,
        requestedOwnerId = payload.requestedOwnerId,
        generationNumber = payload.requestedGenerationNumber
      }
  toVariant eventValue = case eventValue of
    GeneratedPdfRequestRecorded {entityId, requestedOwnerId, generationNumber} ->
      Just
        GeneratedPdfRequestedPayload
          { requestedDocumentId = entityId,
            requestedOwnerId,
            requestedGenerationNumber = generationNumber
          }
    GeneratedPdfReadyRecorded {} -> Nothing
