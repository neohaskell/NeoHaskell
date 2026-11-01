-- | Compose testbed Features and configure their shared application runtime.
-- Run 'appEventModel' or 's3AppEventModel' through 'Service.EventModel.run'.
module Testbed.EventModel (appEventModel, s3AppEventModel) where

import Core
import Service.Application (ApiInfo (..))
import Service.EventModel qualified as EventModel
import Service.FileUpload.BlobStore (BlobStoreConfig)
import Service.FileUpload.Core (FileUploadConfig)
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Feature (cartFlow)
import Testbed.Config (TestbedConfig)
import Testbed.Document.Feature (documentFlow)
import Testbed.Erasable.Feature (protectedFlow)
import Testbed.Erasable.Owned.Feature (ownedErasureFlow)
import Testbed.ExternalEffect.Feature (externalEffectFlow)
import Testbed.Infrastructure (makeFileUploadConfig, makePostgresConfig, makeS3FileUploadConfig)
import Testbed.Stock.Feature (stockFlow)


appEventModel :: Int -> Result Text EventModel.EventModel
appEventModel httpPort = testbedEventModel httpPort makeFileUploadConfig


s3AppEventModel :: Int -> Result Text EventModel.EventModel
s3AppEventModel httpPort = testbedEventModel httpPort makeS3FileUploadConfig


testbedEventModel ::
  forall blobStoreConfig.
  (BlobStoreConfig blobStoreConfig) =>
  Int ->
  (TestbedConfig -> FileUploadConfig blobStoreConfig) ->
  Result Text EventModel.EventModel
testbedEventModel httpPort uploadFactory = EventModel.eventModel do
  EventModel.appName "Testbed"
  EventModel.withConfig @TestbedConfig
  EventModel.withEventStore makePostgresConfig
  EventModel.withTransport (WebTransport.server {WebTransport.port = httpPort})
  EventModel.withApiInfo @() (\_ -> testbedApiInfo)
  EventModel.withFileUpload uploadFactory
  EventModel.features do
    cartFlow
    stockFlow
    documentFlow
    externalEffectFlow
    protectedFlow
    ownedErasureFlow


testbedApiInfo :: ApiInfo
testbedApiInfo =
  ApiInfo
    { apiTitle = "Testbed API",
      apiVersion = "1.0.0",
      apiDescription = "Example NeoHaskell application demonstrating event sourcing, CQRS, and integrations"
    }
