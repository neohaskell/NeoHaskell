-- | Record the retained FileRef produced by the generated PDF integration.
-- Register this command in a Feature for the internal transport.
module Testbed.GeneratedPdf.Commands.Record (
  RecordGeneratedPdf (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Integration (FileRef)
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.GeneratedPdf.Core (GeneratedPdfEntity, GeneratedPdfEvent (..))


data RecordGeneratedPdf = RecordGeneratedPdf
  { documentId :: Uuid,
    fileRef :: FileRef,
    generationNumber :: Int
  }


getEntityId :: RecordGeneratedPdf -> Maybe Uuid
getEntityId command = Just command.documentId


decide :: RecordGeneratedPdf -> Maybe GeneratedPdfEntity -> RequestContext -> Decision GeneratedPdfEvent
decide command maybeEntity _ = case maybeEntity of
  Nothing -> Decider.reject "PDF request was not found"
  Just _ ->
    Decider.acceptExisting
      [ GeneratedPdfReadyRecorded
          { entityId = command.documentId,
            fileRef = command.fileRef,
            generationNumber = command.generationNumber
          }
      ]


type instance EntityOf RecordGeneratedPdf = GeneratedPdfEntity


type instance TransportsOf RecordGeneratedPdf = '[InternalTransport]


command ''RecordGeneratedPdf
