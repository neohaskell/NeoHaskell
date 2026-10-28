-- | Check the timestamp fields and JSON encoding used by a testbed shipment view.
-- Run the nhtestbed-datetime-schema test suite to verify the example.
module Main (main) where

import Core
import DateTime qualified
import Json qualified
import Prelude (IO)
import Schema.JsonSchema qualified
import Test
import Test.Hspec qualified as Hspec


data ShipmentTimeline = ShipmentTimeline
  { shippedAt :: DateTime
  , deliveredAt :: Maybe DateTime
  }
  deriving (Generic)


instance Json.ToJSON ShipmentTimeline


instance ToSchema ShipmentTimeline


main :: IO ()
main = Hspec.hspec do
  describe "Shipment timestamp schema" do
    it "publishes timestamp strings and encodes a shipment" \_ -> do
      let schema = toSchema @ShipmentTimeline
      schema
        |> shouldBe
          (SObject
            [ FieldSchema "shippedAt" SText True ""
            , FieldSchema "deliveredAt" (SOptional SText) False ""
            ])
      Schema.JsonSchema.toJsonSchema schema
        |> shouldBe
          (Json.object
            [ ("type", Json.toJSON ("object" :: Text))
            , ("properties", Json.object
                [ ("shippedAt", Json.object [("type", Json.toJSON ("string" :: Text))])
                , ("deliveredAt", Json.object [("type", Json.toJSON ("string" :: Text))])
                ])
            , ("required", Json.toJSON (["shippedAt"] :: [Text]))
            , ("additionalProperties", Json.toJSON False)
            ])
      let shipment = ShipmentTimeline (DateTime.fromEpochSeconds 0) Nothing
      Json.encode shipment
        |> shouldBe
          (Json.object
            [ ("shippedAt", Json.toJSON ("1970-01-01T00:00:00Z" :: Text))
            , ("deliveredAt", Json.toJSON (Nothing :: Maybe Text))
            ])
