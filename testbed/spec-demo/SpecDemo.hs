{-| Assert the public Event Model and containment APIs in a runnable application test.
Run these examples through the @nhtestbed-spec-demo@ test suite.
-}
module Main (main, ObserveItemAdded (..), CrossEntityView (..)) where

import Core
import App qualified
import Array qualified
import Data.Dynamic qualified as Dynamic
import Integration qualified
import Map qualified
import Service.EventModel qualified as EventModel
import Test
import Test.Hspec qualified as Hspec
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Core (CartCreatedPayload (..), CartEntity (..), ItemAddedPayload (..))
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary (..))
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Core (StockEntity (..), StockInitializedPayload)
import Text qualified
import Uuid qualified


data ObserveItemAdded = ObserveItemAdded


type instance EntityOf ObserveItemAdded = CartEntity


instance OutboundIntegration ObserveItemAdded where
  type HandledEvent ObserveItemAdded = ItemAddedPayload
  handleEventImpl _ _ = Integration.none


data CrossEntityView = CrossEntityView


type instance EntitiesOf CrossEntityView = '[CartEntity, StockEntity]


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
    it "composes executable Features and all four Slice shapes" \_ -> do
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

    it "keeps direct rules with concrete history, command, and ordered event values" \_ -> do
      case App.appEventModel of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let selected = model |> modelSlices |> Array.takeIf (\item -> EventModel.sliceName item == "Add an item")
          case Array.first selected of
            Nothing -> False |> shouldBe True
            Just selectedSlice -> do
              selectedSlice |> EventModel.sliceRuleNames |> shouldBe ["Adding an item emits its exact event", "An unknown cart rejects the command", "A missing cart reason matches its pattern"]
              let samples = EventModel.sampleSliceRules [[Dynamic.toDyn (-3 :: Int)], [], []] selectedSlice
              case samples of
                [Ok accepted, Ok rejectedCase, Ok regexCase] -> do
                  EventModel.sampleRuleName accepted |> shouldBe "Adding an item emits its exact event"
                  case EventModel.sampleRuleHistory accepted of
                    [historical] -> case Dynamic.fromDynamic @CartCreatedPayload historical of
                      Just created -> created.cartCreatedOwnerId |> shouldBe "owner"
                      Nothing -> False |> shouldBe True
                    _ -> False |> shouldBe True
                  case Dynamic.fromDynamic @AddItem (EventModel.sampleRuleWhen accepted) of
                    Just commandValue -> do
                      commandValue.quantity |> shouldBe 8
                      case Dynamic.fromDynamic @(EventModel.CommandExpectation CartEntity) (EventModel.sampleRuleOutcome accepted) of
                        Just (EventModel.Emitted [expected]) -> case Dynamic.fromDynamic @ItemAddedPayload (EventModel.expectedEventValue expected) of
                          Just emitted -> emitted.itemAddedQuantity |> shouldBe commandValue.quantity
                          Nothing -> False |> shouldBe True
                        _ -> False |> shouldBe True
                    Nothing -> False |> shouldBe True
                  case Dynamic.fromDynamic @(EventModel.CommandExpectation CartEntity) (EventModel.sampleRuleOutcome rejectedCase) of
                    Just (EventModel.RejectedReason (Just reason)) -> reason |> shouldBe "Cart not found!"
                    _ -> False |> shouldBe True
                  case Dynamic.fromDynamic @(EventModel.CommandExpectation CartEntity) (EventModel.sampleRuleOutcome regexCase) of
                    Just (EventModel.RejectedPattern patternText) -> patternText |> shouldBe "^Cart not found!$"
                    _ -> False |> shouldBe True
                _ -> False |> shouldBe True

    it "keeps view rules with their exact query actions" \_ -> do
      case App.appEventModel of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let selected = model |> modelSlices |> Array.takeIf (\item -> EventModel.sliceName item == "Show cart summary")
          case Array.first selected of
            Nothing -> False |> shouldBe True
            Just selectedSlice -> do
              selectedSlice |> EventModel.sliceRuleNames |> shouldBe ["A new cart has an empty summary", "An added item changes the summary"]
              let samples = EventModel.sampleSliceRules [[], []] selectedSlice
              case samples of
                [Ok createdCase, Ok addedCase] -> do
                  case Dynamic.fromDynamic @(QueryAction CartSummary) (EventModel.sampleRuleOutcome createdCase) of
                    Just (Update summary) -> summary.itemCount |> shouldBe 0
                    _ -> False |> shouldBe True
                  case Dynamic.fromDynamic @(QueryAction CartSummary) (EventModel.sampleRuleOutcome addedCase) of
                    Just (Update summary) -> summary.itemCount |> shouldBe 1
                    _ -> False |> shouldBe True
                _ -> False |> shouldBe True

    it "rejects a view rule whose when payload is absent from onEvent" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "Missing subscription"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Read" do
                  EventModel.slice "Summary" do
                    EventModel.query @CartSummary do
                      EventModel.readsFrom @CartEntity do
                        EventModel.onEvent @CartCreatedPayload
                    EventModel.rule "Undeclared incoming event" do
                      EventModel.givenNothing
                      EventModel.when ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
                      EventModel.therefore do
                        EventModel.noOp @CartSummary
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> case Array.first (model |> modelSlices) of
          Nothing -> False |> shouldBe True
          Just selectedSlice -> do
            let samples = EventModel.sampleSliceRules [[]] selectedSlice
            case samples of
              [Err problem] -> problem |> Text.contains "Rule \"Undeclared incoming event\" when payload ItemAddedPayload is not listed" |> shouldBe True
              _ -> False |> shouldBe True

    it "checks the query action and complete snapshot diff" \_ -> do
      let before = Map.fromArray [(Uuid.nil, 1 :: Int)]
      let removed = EventModel.ViewObservation
            { action = Delete,
              diff = EventModel.QueryDiff
                { before = EventModel.QuerySnapshot before,
                  after = EventModel.QuerySnapshot Map.empty
                }
            }
      EventModel.verifyViewObservation Uuid.nil Delete removed |> shouldBe True
      EventModel.verifyViewObservation Uuid.nil NoOp removed |> shouldBe False
      let unchanged = EventModel.ViewObservation
            { action = NoOp,
              diff = EventModel.QueryDiff
                { before = EventModel.QuerySnapshot before,
                  after = EventModel.QuerySnapshot before
                }
            }
      EventModel.verifyViewObservation Uuid.nil NoOp unchanged |> shouldBe True
      case Uuid.fromText "00000000-0000-0000-0000-000000000001" of
        Nothing -> False |> shouldBe True
        Just otherId -> do
          let updated = EventModel.ViewObservation
                { action = Update 2,
                  diff = EventModel.QueryDiff
                    { before = EventModel.QuerySnapshot before,
                      after = EventModel.QuerySnapshot (before |> Map.set Uuid.nil 2)
                    }
                }
          EventModel.verifyViewObservation Uuid.nil (Update 2) updated |> shouldBe True
          let changedAnotherRow = EventModel.ViewObservation
                { action = Update 2,
                  diff = EventModel.QueryDiff
                    { before = EventModel.QuerySnapshot before,
                      after = EventModel.QuerySnapshot (before |> Map.set Uuid.nil 2 |> Map.set otherId 3)
                    }
                }
          EventModel.verifyViewObservation Uuid.nil (Update 2) changedAnotherRow |> shouldBe False

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
