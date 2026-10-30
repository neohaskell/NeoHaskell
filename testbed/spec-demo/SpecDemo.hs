{-| Assert the public Event Model and containment APIs in a runnable application test.
Run these examples through the @nhtestbed-spec-demo@ test suite.
-}
module Main (main, ObserveItemAdded (..), CrossEntityView (..)) where

import Core
import App qualified
import Array qualified
import Integration qualified
import Service.EventModel qualified as EventModel
import Test
import Test.Hspec qualified as Hspec
import Testbed.Cart.Commands.AddItem (AddItem)
import Testbed.Cart.Core (CartCreatedPayload, CartEntity (..), ItemAddedPayload)
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Queries.StockLevel (StockLevel)
import Testbed.Stock.Core (StockEntity (..), StockInitializedPayload)
import Text qualified


data ObserveItemAdded = ObserveItemAdded


type instance EntityOf ObserveItemAdded = CartEntity


instance OutboundIntegration ObserveItemAdded where
  type HandledEvent ObserveItemAdded = ItemAddedPayload
  handleEventImpl _ _ = Integration.none


data CrossEntityView = CrossEntityView


instance Query CrossEntityView where
  canAccessImpl _ = Nothing
  canViewImpl _ _ = Nothing


instance QueryOf CartEntity CrossEntityView where
  queryId cart = cart.cartId
  combine _ _ = NoOp


instance QueryOf StockEntity CrossEntityView where
  queryId stock = stock.stockId
  combine _ _ = NoOp


main :: IO ()
main = Hspec.hspec spec


spec :: Spec Unit
spec = do
  describe "source-local Event Model" do
    it "composes both exported Features and all four Slice shapes" \_ -> do
      case App.appEventModel of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          model |> EventModel.modelName |> shouldBe "Testbed"
          let foundFeatures = model |> EventModel.modelFeatures |> Array.fromLinkedList
          foundFeatures |> Array.map EventModel.featureName |> shouldBe ["Cart flow", "Stock flow"]
          let allChapters = foundFeatures |> Array.flatMap (\selected -> selected |> EventModel.featureChapters |> Array.fromLinkedList)
          allChapters |> Array.map EventModel.chapterName |> shouldBe ["Manage carts", "Track availability"]
          let allSlices = allChapters |> Array.flatMap (\selected -> selected |> EventModel.chapterSlices |> Array.fromLinkedList)
          allSlices |> Array.map EventModel.sliceKind |> shouldBe ["state-change", "state-view", "outbound", "inbound", "state-change", "state-view"]
          allSlices |> Array.map EventModel.sliceName |> shouldBe ["Add an item", "Show cart summary", "Reserve stock after an item", "Accept typed inbound commands", "Initialize stock", "Show stock level"]

    it "keeps each translation command with its own target and explicit triggers" \_ -> do
      case App.appEventModel of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let allSlices = model |> modelSlices
          let outbound = allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "outbound")
          let inbound = allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "inbound")
          let cartView = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Show cart summary")
          let addItem = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Add an item")
          let initializeStock = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Initialize stock")
          cartView |> Array.flatMap (\selected -> selected |> EventModel.sliceEventSources |> Array.fromLinkedList) |> shouldBe [("CartEntity", ["CartCreatedPayload", "ItemAddedPayload"])]
          addItem |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList) |> shouldBe ["ItemAddedPayload"]
          initializeStock |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList) |> shouldBe ["StockInitializedPayload"]
          outbound |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList) |> shouldBe ["ItemAddedPayload"]
          outbound |> Array.flatMap (\selected -> selected |> EventModel.sliceCommandTargets |> Array.fromLinkedList) |> shouldBe [("ReserveStock", "StockEntity")]
          inbound |> Array.flatMap (\selected -> selected |> EventModel.sliceCommandTargets |> Array.fromLinkedList) |> shouldBe [("CreateTypedInboundCart", "CartEntity"), ("InitializeStockInternal", "StockEntity")]

    it "rejects an outbound Slice with two triggers" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "Double trigger" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      EventModel.onEvent @ItemAddedPayload
                      EventModel.onEvent @ItemAddedPayload
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Double trigger\" requires exactly one outbound onEvent; found 2" |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "rejects an outbound Slice without a trigger" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "Missing trigger" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      pass
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Missing trigger\" requires exactly one outbound onEvent; found 0" |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "rejects reuse of one outbound handler type across Slices" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "First" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      EventModel.onEvent @ItemAddedPayload
                  EventModel.slice "Second" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      EventModel.onEvent @ItemAddedPayload
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Second\" reuses outbound handler ReserveStockOnItemAdded" |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "preserves distinct outbound handlers on one payload" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "Two handlers"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "Reserve" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      EventModel.onEvent @ItemAddedPayload
                  EventModel.slice "Observe" do
                    EventModel.outboundIntegration @ObserveItemAdded do
                      EventModel.onEvent @ItemAddedPayload
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let allSlices = model |> modelSlices
          allSlices |> Array.map EventModel.sliceName |> shouldBe ["Reserve", "Observe"]
          allSlices |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList) |> shouldBe ["ItemAddedPayload", "ItemAddedPayload"]

    it "allows one inbound handler to appear in separate Slices" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "Repeated inbound"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "First inbound" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      pass
                  EventModel.slice "Second inbound" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      pass
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> model |> modelSlices |> Array.map EventModel.sliceName |> shouldBe ["First inbound", "Second inbound"]

    it "keeps separate explicit subscriptions for a multi-entity query" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "Two sources"
            EventModel.features do
              EventModel.feature "Summary" do
                EventModel.chapter "Read" do
                  EventModel.slice "Combined view" do
                    EventModel.query @CrossEntityView do
                      EventModel.readsFrom @CartEntity do
                        EventModel.onEvent @CartCreatedPayload
                      EventModel.readsFrom @StockEntity do
                        EventModel.onEvent @StockInitializedPayload
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let allSources = model |> modelSlices |> Array.flatMap (\selected -> selected |> EventModel.sliceEventSources |> Array.fromLinkedList)
          allSources |> shouldBe [("CartEntity", ["CartCreatedPayload"]), ("StockEntity", ["StockInitializedPayload"])]

    it "requires Chapters inside exported Features" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Empty" do
                pass
      case invalid of
        Err problem -> problem |> shouldBe "Feature \"Empty\" requires a Chapter"
        Ok _ -> False |> shouldBe True

    it "requires Slices inside Chapters" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Empty" do
                  pass
      case invalid of
        Err problem -> problem |> shouldBe "Feature \"Cart\", Chapter \"Empty\" requires a Slice"
        Ok _ -> False |> shouldBe True

    it "rejects a Slice with no primary" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Commands" do
                  EventModel.slice "Missing" do
                    pass
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Missing\" requires exactly one primary; found 0" |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "rejects two primary patterns in one Slice" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Commands" do
                  EventModel.slice "Two roots" do
                    EventModel.entity @CartEntity do
                      EventModel.command @AddItem do
                        pass
                    EventModel.query @CartSummary do
                      pass
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Two roots\" requires exactly one primary; found 2" |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "rejects two primary queries in one Slice" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Views" do
                EventModel.chapter "Queries" do
                  EventModel.slice "Two queries" do
                    EventModel.query @CartSummary do
                      pass
                    EventModel.query @StockLevel do
                      pass
      case invalid of
        Err problem -> problem |> Text.contains "Slice \"Two queries\" requires exactly one primary; found 2" |> shouldBe True
        Ok _ -> False |> shouldBe True

  describe "public containment matchers" do
    it "finds a value in the piped array" \_ -> do
      (["draft", "review", "published"] :: Array Text)
        |> shouldContain ["review"]

    it "accepts a longer missing sequence" \_ -> do
      (["draft", "review"] :: Array Text)
        |> shouldNotContain ["draft", "review", "published"]


modelSlices :: EventModel.EventModel -> Array EventModel.Slice
modelSlices model =
  model
    |> EventModel.modelFeatures
    |> Array.fromLinkedList
    |> Array.flatMap (\selected -> selected |> EventModel.featureChapters |> Array.fromLinkedList)
    |> Array.flatMap (\selected -> selected |> EventModel.chapterSlices |> Array.fromLinkedList)
