-- | Declare the authenticated request, generated file result, query, and outbound worker.
-- Compose this Feature in the OAuth Event Model after enabling file upload.
module Testbed.GeneratedPdf.Feature (generatedPdfFlow) where

import Service.EventModel
import Testbed.GeneratedPdf.Commands.Record (RecordGeneratedPdf)
import Testbed.GeneratedPdf.Commands.Request (RequestGeneratedPdf)
import Testbed.GeneratedPdf.Core (GeneratedPdfEntity, GeneratedPdfEvent, GeneratedPdfRequestedPayload)
import Testbed.GeneratedPdf.Outbound (GeneratedPdfOutbound)
import Testbed.GeneratedPdf.Query (GeneratedPdfView)


generatedPdfFlow :: FeatureAction
generatedPdfFlow = feature "Generated PDF flow" do
  chapter "Generate an owned PDF" do
    slice "Request a generated PDF" do
      entity @GeneratedPdfEntity do
        command @RequestGeneratedPdf do
          produces @GeneratedPdfRequestedPayload

    slice "Record a generated PDF" do
      entity @GeneratedPdfEntity do
        command @RecordGeneratedPdf do
          produces @GeneratedPdfEvent

    slice "Show generated PDFs" do
      query @GeneratedPdfView do
        readsFrom @GeneratedPdfEntity do
          onEvent @GeneratedPdfEvent

    slice "Generate and retain the PDF" do
      outboundIntegration @GeneratedPdfOutbound do
        onEvent @GeneratedPdfRequestedPayload
        triggersCommand @RecordGeneratedPdf do
          onTargetEntity @GeneratedPdfEntity
