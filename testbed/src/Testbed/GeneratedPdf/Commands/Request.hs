-- | Accept an authenticated request to generate or retry an owned PDF.
-- Register this command in a Feature for the Web transport.
module Testbed.GeneratedPdf.Commands.Request (
  RequestGeneratedPdf (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Service.Auth (RequestContext (..), UserClaims (..))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.GeneratedPdf.Core


data RequestGeneratedPdf = RequestGeneratedPdf
  { documentId :: Uuid
  }


getEntityId :: RequestGeneratedPdf -> Maybe Uuid
getEntityId command = Just command.documentId


decide :: RequestGeneratedPdf -> Maybe GeneratedPdfEntity -> RequestContext -> Decision GeneratedPdfEvent
decide command maybeEntity context = case context.user of
  Nothing -> Decider.reject "Authentication required"
  Just user -> case maybeEntity of
    Just entity
      | entity.ownerId != user.sub -> Decider.reject "PDF request is not owned by this user"
    _ -> do
      let generationNumber = case maybeEntity of
            Nothing -> 1
            Just entity -> entity.requestCount + 1
      let event =
            GeneratedPdfRequestRecorded
              { entityId = command.documentId,
                requestedOwnerId = user.sub,
                generationNumber
              }
      case maybeEntity of
        Nothing -> Decider.acceptNew [event]
        Just _ -> Decider.acceptExisting [event]


type instance EntityOf RequestGeneratedPdf = GeneratedPdfEntity


type instance TransportsOf RequestGeneratedPdf = '[WebTransport]


command ''RequestGeneratedPdf
