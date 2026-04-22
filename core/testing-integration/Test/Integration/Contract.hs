{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Auto-generated contract-test harness.
--
-- For every 'Integration' instance, asserts that fake output parses under
-- the declared 'Response' schema.
module Test.Integration.Contract (contractTests) where

import Basics
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Result (Result (..))
import Service.Integration.Adapter (Integration (..))
import Service.Integration.IntegrationError (IntegrationError)
import Task (Task)
import Task qualified
import Test.Hspec qualified as Hspec
import Test.QuickCheck qualified as QuickCheck
import Text (Text)
import Text qualified
import Prelude qualified


-- | Build an Hspec test tree for a single integration instance.
contractTests ::
  forall request.
  ( Integration request,
    QuickCheck.Arbitrary request,
    Show request,
    Aeson.ToJSON (Response request),
    Aeson.FromJSON (Response request)
  ) =>
  Text ->
  Hspec.SpecWith ()
contractTests integrationName = do
  Hspec.describe (Text.toLinkedList [fmt|contract: #{integrationName}|]) do
    Hspec.it "fake output parses under the current Response schema" do
      QuickCheck.quickCheck
        ( QuickCheck.property
            ( \(req :: request) ->
                QuickCheck.ioProperty
                  ( do
                      let task :: Task IntegrationError Bool
                          task = do
                            response <- runFake req
                            Task.yield (roundTrips response)
                      let asResultTask :: Task IntegrationError (Result IntegrationError Bool)
                          asResultTask = Task.asResult task
                      result <- Task.runOrPanic asResultTask
                      case result of
                        Err _ -> Prelude.return False
                        Ok ok -> Prelude.return ok
                  )
            )
        )


roundTrips ::
  forall response.
  (Aeson.ToJSON response, Aeson.FromJSON response) =>
  response ->
  Bool
roundTrips response =
  case Aeson.eitherDecodeStrict (LazyByteString.toStrict (Aeson.encode response)) of
    Prelude.Left _ -> False
    Prelude.Right (_ :: response) -> True
