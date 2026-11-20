-- | Expose a generated document only to its authenticated owner.
-- Register this query in a Feature that reads the generated PDF entity.
module Testbed.GeneratedPdf.Query (
  GeneratedPdfView (..),
  canAccess,
  canView,
) where

import Core
import Integration (FileRef)
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.GeneratedPdf.Core


data GeneratedPdfView = GeneratedPdfView
  { documentId :: Uuid,
    ownerId :: Text,
    status :: Text,
    fileRef :: Maybe FileRef,
    completedAttempts :: Int
  }
  deriving (Eq, Generic, Show)


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess


canView :: Maybe UserClaims -> GeneratedPdfView -> Maybe AccessError
canView claims view = AccessControl.ownerOnly (\owned -> owned.ownerId) claims view


deriveQuery ''GeneratedPdfView [''GeneratedPdfEntity]


instance QueryOf GeneratedPdfEntity GeneratedPdfView where
  queryId = (.documentId)
  combine entity _ =
    let (status, fileRef) = case entity.fileRef of
          Nothing -> ("requested", Nothing)
          Just ref -> ("ready", Just ref)
    in  Update
          GeneratedPdfView
            { documentId = entity.documentId,
              ownerId = entity.ownerId,
              status,
              fileRef,
              completedAttempts = entity.completedAttempts
            }
