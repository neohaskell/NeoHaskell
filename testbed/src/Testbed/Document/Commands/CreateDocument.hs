module Testbed.Document.Commands.CreateDocument (
  CreateDocument (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Map qualified
import Service.Auth (RequestContext (..))
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Service.FileUpload.Core (FileRef)
import Service.Transport.Web (WebTransport)
import Testbed.Document.Core


-- | Create a document with an attached file.
-- The attachment field contains a FileRef from a previous upload.
-- The framework resolves and validates this FileRef before the command runs.
data CreateDocument = CreateDocument
  { documentId :: Uuid
  , title :: Text
  , attachment :: FileRef
  -- ^ Reference to previously uploaded file (validated by framework)
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateDocument


getEntityId :: CreateDocument -> Maybe Uuid
getEntityId cmd = Just cmd.documentId


-- | Decide whether to create the document.
-- The attachment is validated by the framework before this runs,
-- so we can safely use it here. The resolved file is in ctx.files.
decide :: CreateDocument -> Maybe DocumentEntity -> RequestContext -> Decision DocumentEvent
decide cmd entity ctx = case entity of
  Just _ ->
    Decider.reject "Document already exists"
  Nothing -> do
    -- The framework has already validated the FileRef exists and belongs to this user.
    -- We can access the resolved file metadata from ctx.files if needed:
    let maybeFile = ctx.files |> Map.get cmd.attachment
    case maybeFile of
      Nothing ->
        -- This shouldn't happen if framework validation passed,
        -- but we handle it defensively
        Decider.reject "Attachment file not found in context"
      Just _resolvedFile -> do
        -- Use current timestamp (in a real app, would come from ctx or event metadata)
        let nowEpoch = 1705000000 -- Placeholder, should use DateTime in production
        Decider.acceptNew
          [ DocumentCreated
              { entityId = cmd.documentId
              , title = cmd.title
              , attachmentRef = cmd.attachment
              , createdAt = nowEpoch
              }
          ]


type instance EntityOf CreateDocument = DocumentEntity


type instance TransportOf CreateDocument = WebTransport


command ''CreateDocument
