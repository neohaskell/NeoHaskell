{-# LANGUAGE TemplateHaskell #-}

-- | Render a deterministic PDF and retain it through the application's file capability.
-- Register this handler in a Feature that listens for GeneratedPdfRequested.
module Testbed.GeneratedPdf.Outbound (
  GeneratedPdfOutbound (..),
  handleEvent,
) where

import Bytes qualified
import Core
import Integration (CreateFileRequest (..))
import Integration qualified
import Service.OutboundIntegration.TH (outboundIntegration)
import Task qualified
import Testbed.GeneratedPdf.Commands.Record (RecordGeneratedPdf (..))
import Testbed.GeneratedPdf.Core
import Uuid qualified


data GeneratedPdfOutbound = GeneratedPdfOutbound
  deriving (Generic, Typeable, Show)


type instance EntityOf GeneratedPdfOutbound = GeneratedPdfEntity


handleEvent :: GeneratedPdfEntity -> GeneratedPdfRequestedPayload -> Integration.Outbound
handleEvent _ payload =
  let documentId = payload.requestedDocumentId
  in  Integration.batch
        [ Integration.action \context -> do
            fileAccess <- case context.fileAccess of
              Nothing -> Task.throw (Integration.ValidationError "File uploads are not enabled")
              Just access -> Task.yield access
            fileRef <-
              fileAccess.createFile
                CreateFileRequest
                  { owner = payload.requestedOwnerId,
                    creationKey = [fmt|generated-pdf-v1-#{Uuid.toText documentId}|],
                    filename = "generated-document.pdf",
                    contentType = "application/pdf",
                    content = generatedPdf
                  }
                |> Task.mapError (\_ -> Integration.UnexpectedError "Generated PDF storage failed")
            Integration.emitCommand
              RecordGeneratedPdf
                { documentId,
                  fileRef,
                  generationNumber = payload.requestedGenerationNumber
                }
        ]


generatedPdf :: Bytes.Bytes
generatedPdf =
  Bytes.pack
    [ 37,
      80,
      68,
      70,
      45,
      49,
      46,
      52,
      13,
      10,
      37,
      32,
      0,
      78,
      101,
      111,
      72,
      97,
      115,
      107,
      101,
      108,
      108,
      32,
      103,
      101,
      110,
      101,
      114,
      97,
      116,
      101,
      100,
      32,
      100,
      111,
      99,
      117,
      109,
      101,
      110,
      116,
      13,
      10,
      37,
      37,
      69,
      79,
      70,
      13,
      10
    ]


outboundIntegration ''GeneratedPdfOutbound
