-- | Assert the public Event Model and containment APIs in a runnable application test.
-- Run these examples through the @nhtestbed-spec-demo@ test suite.
module Main (main, ObserveItemAdded (..), CrossEntityView (..)) where

import Array qualified
import AsyncTask qualified
import Auth.Config qualified as AuthConfig
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Core
import Data.Dynamic qualified as Dynamic
import Integration qualified
import Map qualified
import Prelude qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.Application qualified as Application
import Service.EventModel qualified as EventModel
import Service.EventModel.RuleRunner qualified as RuleRunner
import Service.Integration.Adapter (Integration (..))
import Service.Integration.Selection (Selection (..))
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Transport.Web qualified as WebTransport
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Test.QuickCheck qualified as QuickCheck
import Testbed.Cart.Commands.AddItem (AddItem (..))
import Testbed.Cart.Commands.AddTwoItems (AddTwoItems (..))
import Testbed.Cart.Commands.ConfirmCart (ConfirmCart (..))
import Testbed.Cart.Commands.OverfillCart (OverfillCart (..))
import Testbed.Cart.Commands.CreateTypedInboundCart (CreateTypedInboundCart)
import Testbed.Cart.Core (CartCreatedPayload (..), CartEntity (..), IBANSavedPayload (..), ItemAddedPayload (..))
import Testbed.Cart.Feature (cartFlow)
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary (..))
import Testbed.Cart.Queries.RuleProjection (RuleProjection (..), projectionId)
import Testbed.Config (TestbedConfig)
import Testbed.EventModel qualified as App
import Testbed.Integrations.TypedInboundExample (TypedInboundExample)
import Testbed.Stock.Commands.InitializeStockInternal (InitializeStockInternal)
import Testbed.Stock.Core (StockEntity (..), StockInitializedPayload (..))
import Text qualified
import Uuid qualified


data ObserveItemAdded = ObserveItemAdded


data ProbeRequest = ProbeRequest


instance Integration ProbeRequest where
  type Response ProbeRequest = Unit
  runReal _ = Task.yield unit
  runFake _ = Task.yield unit


type instance EntityOf ObserveItemAdded = CartEntity


instance OutboundIntegration ObserveItemAdded where
  type HandledEvent ObserveItemAdded = ItemAddedPayload
  handleEventImpl _ _ = Integration.none


data CrossEntityView = CrossEntityView
  { crossEntityId :: Uuid,
    crossEntityCount :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess _ = Nothing


canView :: Maybe UserClaims -> CrossEntityView -> Maybe AccessError
canView _ _ = Nothing


deriveQuery ''CrossEntityView [''CartEntity, ''StockEntity]


instance QueryOf CartEntity CrossEntityView where
  queryId cart =
    if cart.savedIBAN == Just "wrong-row" || cart.savedIBAN == Just "wrong-delete" || cart.savedIBAN == Just "wrong-noop"
      then wrongRowId
      else cart.cartId
  combine cart _ = case cart.savedIBAN of
    Just "wrong-delete" -> Delete
    Just "wrong-noop" -> NoOp
    _ -> Update CrossEntityView {crossEntityId = cart.cartId, crossEntityCount = Array.length cart.items}


instance QueryOf StockEntity CrossEntityView where
  queryId stock = stock.stockId
  combine _ _ = NoOp


main :: IO ()
main = Hspec.hspec spec


spec :: Spec Unit
spec = do
  case App.appEventModel "127.0.0.1" 8080 of
    Err problem -> Hspec.it "assembles direct rules" do
      Hspec.expectationFailure (Text.toLinkedList problem)
    Ok model -> do
      let generators = RuleRunner.emptyGenerators
            |> RuleRunner.withGenerator @Int (QuickCheck.choose (1, 100)) toText
      RuleRunner.specFor generators 583 3 model

  describe "source-local Event Model" do
    it "retains generated cart and stock IDs across given, when and therefore" \_ -> do
      case bindingRuleModel of
        Err problem -> problem |> shouldBe "valid binding rule model"
        Ok model -> do
          let generators = RuleRunner.emptyGenerators
                |> RuleRunner.withGenerator @Uuid (QuickCheck.elements [Uuid.nil, fixtureRowId]) Uuid.toText
                |> RuleRunner.withGenerator @Int (QuickCheck.choose (1, 5)) toText
          reports <- RuleRunner.runRules generators 223 3 model
          reports |> Array.fromLinkedList |> Array.length |> shouldBe 3
          let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          failures |> shouldBe []
          reports |> Prelude.all (\report -> Prelude.length report.inputs == 3) |> shouldBe True

    it "prints generated bindings and a replay seed on failure" \_ -> do
      case generatedFailureModel of
        Err problem -> problem |> shouldBe "valid generated failure model"
        Ok model -> do
          let generators = RuleRunner.emptyGenerators
                |> RuleRunner.withGenerator @Int (QuickCheck.choose (1, 5)) toText
          reports <- RuleRunner.runRules generators 211 1 model
          case reports of
            [report] -> do
              let diagnostic = RuleRunner.reportText report
              diagnostic |> Text.contains "Cart / Commands / Add / generated mismatch" |> shouldBe True
              diagnostic |> Text.contains "seed=211" |> shouldBe True
              diagnostic |> Text.contains "Int=" |> shouldBe True
              diagnostic |> Text.contains "ordered events differ" |> shouldBe True
            _ -> False |> shouldBe True

    it "reports the underlying event-store failure with its rule identity" \_ -> do
      case frameworkFailureModel of
        Err problem -> problem |> shouldBe "valid framework failure model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 67 1 model
          case reports of
            [report] -> do
              let diagnostic = RuleRunner.reportText report
              diagnostic |> Text.contains "Cart / Commands / Overfill / store failure" |> shouldBe True
              diagnostic |> Text.contains "framework error" |> shouldBe True
              diagnostic |> Text.contains "PayloadTooLarge" |> shouldBe True
              diagnostic |> Text.contains "seed=67" |> shouldBe True
            _ -> False |> shouldBe True

    it "isolates overlapping command histories and query rows concurrently" \_ -> do
      case (orderedRuleModel, viewRuleModel) of
        (Ok commandModel, Ok queryModel) -> do
          (commandFirst, commandSecond) <- AsyncTask.runConcurrently
            (RuleRunner.runRules RuleRunner.emptyGenerators 103 2 commandModel, RuleRunner.runRules RuleRunner.emptyGenerators 103 2 commandModel)
          let commandPasses reports = [report | report <- reports, Text.contains "ordered pass" report.identity, case report.result of Ok () -> True; Err _ -> False]
          commandFirst |> commandPasses |> Array.fromLinkedList |> Array.length |> shouldBe 2
          commandSecond |> commandPasses |> Array.fromLinkedList |> Array.length |> shouldBe 2
          (queryFirst, querySecond) <- AsyncTask.runConcurrently
            (RuleRunner.runRules RuleRunner.emptyGenerators 109 2 queryModel, RuleRunner.runRules RuleRunner.emptyGenerators 109 2 queryModel)
          let queryFailures reports = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          queryFirst |> queryFailures |> Array.fromLinkedList |> Array.length |> shouldBe 4
          querySecond |> queryFailures |> Array.fromLinkedList |> Array.length |> shouldBe 4
        _ -> False |> shouldBe True

    it "checks ignored, exact and regex rejections and accepted no-event alternatives" \_ -> do
      case rejectionRuleModel of
        Err problem -> problem |> shouldBe "valid rejection rule model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 29 1 model
          reports |> Array.fromLinkedList |> Array.length |> shouldBe 7
          let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          failures |> Array.fromLinkedList |> Array.length |> shouldBe 4
          let wrong = ["wrong exact reason", "wrong regex reason", "rejection is not accepted without events", "emitted event is not accepted without events"] :: [Text]
          wrong |> Prelude.all (\name -> failures |> Prelude.any (Text.contains name)) |> shouldBe True
          failures |> Prelude.all (Text.contains "seed=") |> shouldBe True

    it "detects missing, extra, reordered and unequal concrete events" \_ -> do
      case orderedRuleModel of
        Err problem -> problem |> shouldBe "valid ordered rule model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 73 1 model
          RuleRunner.declaredRules model |> Array.fromLinkedList |> Array.length |> shouldBe 5
          reports |> Array.fromLinkedList |> Array.length |> shouldBe 5
          let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          failures |> Array.fromLinkedList |> Array.length |> shouldBe 4
          failures |> Prelude.all (\message -> Text.contains "ordered events differ" message && Text.contains "seed=" message && Text.contains "expected" message && Text.contains "actual" message) |> shouldBe True
          let names = ["missing event", "extra event", "reordered events", "unequal event"] :: [Text]
          names |> Prelude.all (\name -> failures |> Prelude.any (Text.contains name)) |> shouldBe True

    it "observes updates, deletion, no-op and the full query diff" \_ -> do
      fixtureRowId == Uuid.nil |> shouldBe False
      case viewRuleModel of
        Err problem -> problem |> shouldBe "valid view rule model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 91 1 model
          RuleRunner.declaredRules model |> Array.fromLinkedList |> Array.length |> shouldBe 5
          reports |> Array.fromLinkedList |> Array.length |> shouldBe 5
          let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          failures |> Array.fromLinkedList |> Array.length |> shouldBe 2
          failures |> Array.fromLinkedList |> Array.any (Text.contains "update with unchanged row is not no-op") |> shouldBe True
          failures |> Array.fromLinkedList |> Array.any (Text.contains "delete needs a prior row") |> shouldBe True
          failures |> Prelude.all (Text.contains "query action or complete diff differs") |> shouldBe True

    it "rejects generated view actions on the wrong row among two projections" \_ -> do
      case twoRowViewRuleModel of
        Err problem -> problem |> shouldBe "valid two-row view model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 97 1 model
          case reports of
            [rightRow, wrongUpdate, wrongDelete, wrongNoOp] -> do
              rightRow.result |> shouldBe (Ok ())
              let wrong = [wrongUpdate, wrongDelete, wrongNoOp] :: [RuleRunner.RuleReport]
              wrong |> Prelude.all (\report -> case report.result of Err problem -> Text.contains "query action or complete diff differs" problem; Ok () -> False) |> shouldBe True
            _ -> False |> shouldBe True

    it "accepts an existing cart without appending an event" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "No event rule"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Commands" do
                  EventModel.slice "Confirm" do
                    EventModel.entity @CartEntity do
                      EventModel.command @ConfirmCart do
                        pass
                    EventModel.rule "A confirmation keeps the history" do
                      EventModel.given do
                        EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
                      EventModel.when ConfirmCart {cartId = Uuid.nil}
                      EventModel.therefore do
                        EventModel.acceptedWithNoEvents
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 19 1 model
          let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          failures |> shouldBe []

    it "runs every direct cart rule through isolated service and query storage" \_ -> do
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let generators = RuleRunner.emptyGenerators
                |> RuleRunner.withGenerator @Int (QuickCheck.choose (1, 100)) toText
          outcome <- RuleRunner.runRules generators 583 3 model |> Task.asResult
          case outcome of
            Err problem -> problem |> shouldBe "rule runner should start"
            Ok reports -> do
              reports |> Array.fromLinkedList |> Array.length |> shouldBe 15
              let exercised = [report.identity | report <- reports, report.sampleIndex == 0]
              exercised |> shouldBe (RuleRunner.declaredRules model)
              let failures = [RuleRunner.reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
              failures |> shouldBe []

    it "generates no examples when the model has no direct rules" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "No direct rules"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Commands" do
                  EventModel.slice "Confirm" do
                    EventModel.entity @CartEntity do
                      EventModel.command @ConfirmCart do
                        pass
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          RuleRunner.declaredRules model |> shouldBe []
          reports <- RuleRunner.runRules RuleRunner.emptyGenerators 31 3 model
          reports |> Array.fromLinkedList |> Array.length |> shouldBe 0

    it "lowers declared components into the application" \_ -> do
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let application = EventModel.toApplication model
          Application.serviceRunnerCount application |> shouldBe 17
          Application.queryDefinitionCount application |> shouldBe 6
          Application.transportCount application |> shouldBe 1
          Application.hasConfig application |> shouldBe True
          Application.hasEventStore application |> shouldBe True
          application.bindHost |> shouldBe (Just "127.0.0.1")
          application.bindPort |> shouldBe (Just 8080)

    it "configures capabilities and typed adapters inside the named model" \_ -> do
      store <- InMemorySecretStore.new
      let modelResult = EventModel.eventModel do
            EventModel.appName "Configured"
            EventModel.withConfig @TestbedConfig
            EventModel.withSecretStore @() (\_ -> store)
            EventModel.withAuthOverrides @() (\_ -> "https://auth.example.test") AuthConfig.defaultOverrides
            EventModel.withCors @() (\_ -> WebTransport.CorsConfig {WebTransport.allowedOrigins = ["https://example.test"], WebTransport.allowedMethods = ["GET"], WebTransport.allowedHeaders = [], WebTransport.maxAge = Nothing})
            EventModel.withHealthCheck @() (\_ -> "live")
            EventModel.useReadinessEndpoint
            EventModel.withIntegration @ProbeRequest
            EventModel.withoutHealthCheck
            EventModel.features do
              cartFlow
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let application = EventModel.toApplication model
          Application.hasConfig application |> shouldBe True
          Application.hasAuth application |> shouldBe True
          case application.secretStoreFactory of
            Nothing -> False |> shouldBe True
            Just _ -> pass
          case application.corsFactory of
            Nothing -> False |> shouldBe True
            Just _ -> pass
          case application.healthCheckConfig of
            Just _ -> False |> shouldBe True
            Nothing -> pass
          case application.readinessConfig of
            Nothing -> False |> shouldBe True
            Just _ -> pass
          let registry = application.integrationRegistrations |> Array.reduce (\entry current -> entry.registerInto Real current) DispatchRegistry.empty
          DispatchRegistry.size registry |> shouldBe 1
          case DispatchRegistry.lookup @ProbeRequest registry of
            Nothing -> False |> shouldBe True
            Just runProbe -> do
              result <- runProbe ProbeRequest |> Task.asResult
              result |> shouldBe (Ok unit)

    it "rejects duplicate configuration before lowering or running effects" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Duplicate configuration"
            EventModel.withConfig @TestbedConfig
            EventModel.withConfig @TestbedConfig
            EventModel.withoutHealthCheck
            EventModel.features do
              cartFlow
      case invalid of
        Ok _ -> False |> shouldBe True
        Err problem -> problem |> Text.contains "at most one withConfig" |> shouldBe True
      result <- EventModel.run invalid |> Task.asResult
      case result of
        Ok _ -> False |> shouldBe True
        Err problem -> problem |> Text.contains "at most one withConfig" |> shouldBe True

    it "composes executable Features and all four Slice shapes" \_ -> do
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          model |> EventModel.modelName |> shouldBe "Testbed"
          let foundFeatures = model |> EventModel.modelFeatures |> Array.fromLinkedList
          foundFeatures
            |> Array.map EventModel.featureName
            |> shouldBe
              ["Cart flow", "Stock flow", "Document flow", "External effect flow", "Protected record flow", "Owned erasure flow"]
          let allChapters = foundFeatures |> Array.flatMap (\selected -> selected |> EventModel.featureChapters |> Array.fromLinkedList)
          allChapters
            |> Array.map EventModel.chapterName
            |> shouldBe
              [ "Manage carts",
                "Track availability",
                "Manage documents",
                "Deliver effects",
                "Store protected values",
                "Erase protected values"
              ]
          let allSlices = allChapters |> Array.flatMap (\selected -> selected |> EventModel.chapterSlices |> Array.fromLinkedList)
          allSlices |> Array.length |> shouldBe 29
          allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "state-change") |> Array.length |> shouldBe 17
          allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "state-view") |> Array.length |> shouldBe 6
          allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "outbound") |> Array.length |> shouldBe 4
          allSlices |> Array.takeIf (\selected -> EventModel.sliceKind selected == "inbound") |> Array.length |> shouldBe 2

    it "keeps direct rules with concrete history, command, and ordered event values" \_ -> do
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let selected = model |> modelSlices |> Array.takeIf (\item -> EventModel.sliceName item == "Add an item")
          case Array.first selected of
            Nothing -> False |> shouldBe True
            Just selectedSlice -> do
              selectedSlice
                |> EventModel.sliceRuleNames
                |> shouldBe
                  [ "Adding an item emits its exact event",
                    "An unknown cart rejects the command",
                    "A missing cart reason matches its pattern"
                  ]
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
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let selected = model |> modelSlices |> Array.takeIf (\item -> EventModel.sliceName item == "Show cart summary")
          case Array.first selected of
            Nothing -> False |> shouldBe True
            Just selectedSlice -> do
              selectedSlice
                |> EventModel.sliceRuleNames
                |> shouldBe ["A new cart has an empty summary", "An added item changes the summary"]
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
                        EventModel.noOp @CartSummary Uuid.nil
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> case Array.first (model |> modelSlices) of
          Nothing -> False |> shouldBe True
          Just selectedSlice -> do
            let samples = EventModel.sampleSliceRules [[]] selectedSlice
            case samples of
              [Err problem] ->
                problem
                  |> Text.contains "Rule \"Undeclared incoming event\" when payload ItemAddedPayload is not listed"
                  |> shouldBe True
              _ -> False |> shouldBe True

    it "checks the query action and complete snapshot diff" \_ -> do
      let before = Map.fromArray [(Uuid.nil, 1 :: Int)]
      let removed =
            EventModel.ViewObservation
              { action = Delete,
                diff =
                  EventModel.QueryDiff
                    { before = EventModel.QuerySnapshot before,
                      after = EventModel.QuerySnapshot Map.empty
                    }
              }
      EventModel.verifyViewObservation Uuid.nil Delete removed |> shouldBe True
      EventModel.verifyViewObservation Uuid.nil NoOp removed |> shouldBe False
      let unchanged =
            EventModel.ViewObservation
              { action = NoOp,
                diff =
                  EventModel.QueryDiff
                    { before = EventModel.QuerySnapshot before,
                      after = EventModel.QuerySnapshot before
                    }
              }
      EventModel.verifyViewObservation Uuid.nil NoOp unchanged |> shouldBe True
      case Uuid.fromText "00000000-0000-0000-0000-000000000001" of
        Nothing -> False |> shouldBe True
        Just otherId -> do
          let updated =
                EventModel.ViewObservation
                  { action = Update 2,
                    diff =
                      EventModel.QueryDiff
                        { before = EventModel.QuerySnapshot before,
                          after = EventModel.QuerySnapshot (before |> Map.set Uuid.nil 2)
                        }
                  }
          EventModel.verifyViewObservation Uuid.nil (Update 2) updated |> shouldBe True
          let changedAnotherRow =
                EventModel.ViewObservation
                  { action = Update 2,
                    diff =
                      EventModel.QueryDiff
                        { before = EventModel.QuerySnapshot before,
                          after = EventModel.QuerySnapshot (before |> Map.set Uuid.nil 2 |> Map.set otherId 3)
                        }
                  }
          EventModel.verifyViewObservation Uuid.nil (Update 2) changedAnotherRow |> shouldBe False

    it "keeps each translation command with its own target and explicit triggers" \_ -> do
      case App.appEventModel "127.0.0.1" 8080 of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> do
          let allSlices = model |> modelSlices
          let outbound = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Reserve stock after an item")
          let inbound = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Accept typed inbound commands")
          let cartView = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Show cart summary")
          let addItem = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Add an item")
          let initializeStock = allSlices |> Array.takeIf (\selected -> EventModel.sliceName selected == "Initialize stock")
          cartView
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventSources |> Array.fromLinkedList)
            |> shouldBe [("CartEntity", ["CartCreatedPayload", "ItemAddedPayload", "IBANSavedPayload"])]
          addItem
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList)
            |> shouldBe ["ItemAddedPayload"]
          initializeStock
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList)
            |> shouldBe ["StockInitializedPayload"]
          outbound
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList)
            |> shouldBe ["ItemAddedPayload"]
          outbound
            |> Array.flatMap (\selected -> selected |> EventModel.sliceCommandTargets |> Array.fromLinkedList)
            |> shouldBe [("ReserveStock", "StockEntity")]
          inbound
            |> Array.flatMap (\selected -> selected |> EventModel.sliceCommandTargets |> Array.fromLinkedList)
            |> shouldBe [("CreateTypedInboundCart", "CartEntity"), ("InitializeStockInternal", "StockEntity")]
          let externalHandlers =
                allSlices
                  |> Array.takeIf
                    ( \selected ->
                        EventModel.sliceName selected
                          == "Deliver a requested external effect"
                          || EventModel.sliceName selected
                          == "Audit a requested external effect"
                    )
          externalHandlers
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList)
            |> shouldBe ["ExternalEffectRequestedPayload", "ExternalEffectRequestedPayload"]

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
          allSlices
            |> Array.flatMap (\selected -> selected |> EventModel.sliceEventTypes |> Array.fromLinkedList)
            |> shouldBe ["ItemAddedPayload", "ItemAddedPayload"]

    it "allows one inbound handler to appear in separate Slices" \_ -> do
      let modelResult = EventModel.eventModel do
            EventModel.appName "Repeated inbound"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "First inbound" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      EventModel.triggersCommand @CreateTypedInboundCart do
                        EventModel.onTargetEntity @CartEntity
                      EventModel.triggersCommand @InitializeStockInternal do
                        EventModel.onTargetEntity @StockEntity
                  EventModel.slice "Second inbound" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      EventModel.triggersCommand @CreateTypedInboundCart do
                        EventModel.onTargetEntity @CartEntity
                      EventModel.triggersCommand @InitializeStockInternal do
                        EventModel.onTargetEntity @StockEntity
      case modelResult of
        Err problem -> problem |> shouldBe "valid Event Model"
        Ok model -> model |> modelSlices |> Array.map EventModel.sliceName |> shouldBe ["First inbound", "Second inbound"]

    it "rejects an incomplete typed inbound output list at model entry" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Incomplete inbound"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "Missing stock output" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      EventModel.triggersCommand @CreateTypedInboundCart do
                        EventModel.onTargetEntity @CartEntity
      case invalid of
        Err problem ->
          problem
            |> Text.contains
              "Feature \"Cart\", Chapter \"Integrations\", Slice \"Missing stock output\" omits inbound worker output InitializeStockInternal -> StockEntity"
            |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "reports each assembled Slice failure before application startup" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Several errors"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "First" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      EventModel.onEvent @ItemAddedPayload
                  EventModel.slice "Second" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      pass
                  EventModel.slice "Incomplete inbound" do
                    EventModel.inboundIntegration @TypedInboundExample do
                      pass
      case invalid of
        Err problem -> do
          problem |> Text.contains "Slice \"Second\" requires exactly one outbound onEvent; found 0" |> shouldBe True
          problem |> Text.contains "Slice \"Second\" reuses outbound handler ReserveStockOnItemAdded" |> shouldBe True
          problem
            |> Text.contains "Slice \"Incomplete inbound\" omits inbound worker output CreateTypedInboundCart -> CartEntity"
            |> shouldBe True
          problem
            |> Text.contains "Slice \"Incomplete inbound\" omits inbound worker output InitializeStockInternal -> StockEntity"
            |> shouldBe True
        Ok _ -> False |> shouldBe True

    it "reports structural and assembled failures across Features and Chapters" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Mixed failures"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Integrations" do
                  EventModel.slice "No primary" do
                    pass
                  EventModel.slice "Missing trigger" do
                    EventModel.outboundIntegration @ReserveStockOnItemAdded do
                      pass
              EventModel.feature "Stock" do
                EventModel.chapter "Commands" do
                  EventModel.slice "No stock primary" do
                    pass
                EventModel.chapter "Notifications" do
                  EventModel.slice "No notification trigger" do
                    EventModel.outboundIntegration @ObserveItemAdded do
                      pass
      case invalid of
        Err problem -> do
          problem
            |> Text.contains "Feature \"Cart\", Chapter \"Integrations\", Slice \"No primary\" requires exactly one primary; found 0"
            |> shouldBe True
          problem
            |> Text.contains
              "Feature \"Cart\", Chapter \"Integrations\", Slice \"Missing trigger\" requires exactly one outbound onEvent; found 0"
            |> shouldBe True
          problem
            |> Text.contains
              "Feature \"Stock\", Chapter \"Commands\", Slice \"No stock primary\" requires exactly one primary; found 0"
            |> shouldBe True
          problem
            |> Text.contains
              "Feature \"Stock\", Chapter \"Notifications\", Slice \"No notification trigger\" requires exactly one outbound onEvent; found 0"
            |> shouldBe True
          problem |> Text.contains "requires a Slice" |> shouldBe False
        Ok _ -> False |> shouldBe True

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

    it "names both constructs when a Slice repeats its primary" \_ -> do
      let invalid = EventModel.eventModel do
            EventModel.appName "Invalid"
            EventModel.features do
              EventModel.feature "Cart" do
                EventModel.chapter "Commands" do
                  EventModel.slice "Repeated command" do
                    EventModel.entity @CartEntity do
                      EventModel.command @AddItem do
                        pass
                    EventModel.entity @CartEntity do
                      EventModel.command @AddItem do
                        pass
      case invalid of
        Err problem ->
          problem
            |> Text.contains "Slice \"Repeated command\" requires exactly one primary; found 2: state-change, state-change"
            |> shouldBe True
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


orderedRuleModel :: Result Text EventModel.EventModel
orderedRuleModel = EventModel.eventModel do
  EventModel.appName "Ordered event rules"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Commands" do
        EventModel.slice "Add two items" do
          EventModel.entity @CartEntity do
            EventModel.command @AddTwoItems do
              EventModel.produces @ItemAddedPayload

          EventModel.rule "ordered pass" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "fixture-only owner"}
            EventModel.when AddTwoItems {cartId = Uuid.nil, firstStockId = Uuid.nil, secondStockId = Uuid.nil}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 2}

          EventModel.rule "missing event" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "fixture-only owner"}
            EventModel.when AddTwoItems {cartId = Uuid.nil, firstStockId = Uuid.nil, secondStockId = Uuid.nil}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}

          EventModel.rule "extra event" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "fixture-only owner"}
            EventModel.when AddTwoItems {cartId = Uuid.nil, firstStockId = Uuid.nil, secondStockId = Uuid.nil}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 2}
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 3}

          EventModel.rule "reordered events" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "fixture-only owner"}
            EventModel.when AddTwoItems {cartId = Uuid.nil, firstStockId = Uuid.nil, secondStockId = Uuid.nil}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 2}
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}

          EventModel.rule "unequal event" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "fixture-only owner"}
            EventModel.when AddTwoItems {cartId = Uuid.nil, firstStockId = Uuid.nil, secondStockId = Uuid.nil}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 1}
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = 3}


frameworkFailureModel :: Result Text EventModel.EventModel
frameworkFailureModel = EventModel.eventModel do
  EventModel.appName "Framework failure rule"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Commands" do
        EventModel.slice "Overfill" do
          EventModel.entity @CartEntity do
            EventModel.command @OverfillCart do
              EventModel.produces @ItemAddedPayload
          EventModel.rule "store failure" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when OverfillCart {cartId = Uuid.nil}
            EventModel.therefore do
              EventModel.acceptedWithNoEvents


generatedFailureModel :: Result Text EventModel.EventModel
generatedFailureModel = EventModel.eventModel do
  EventModel.appName "Generated input diagnostics"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Commands" do
        EventModel.slice "Add" do
          EventModel.entity @CartEntity do
            EventModel.command @AddItem do
              EventModel.produces @ItemAddedPayload
          EventModel.rule "generated mismatch" do
            amount <- EventModel.requestValue @Int
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = amount}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = Uuid.nil, itemAddedStockId = Uuid.nil, itemAddedQuantity = amount + 1}


bindingRuleModel :: Result Text EventModel.EventModel
bindingRuleModel = EventModel.eventModel do
  EventModel.appName "Generated binding rule"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Commands" do
        EventModel.slice "Add" do
          EventModel.entity @CartEntity do
            EventModel.command @AddItem do
              EventModel.produces @ItemAddedPayload
          EventModel.rule "bound values stay paired" do
            cartId <- EventModel.requestValue @Uuid
            stockId <- EventModel.requestValue @Uuid
            amount <- EventModel.requestValue @Int
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = cartId, cartCreatedOwnerId = "owner"}
            EventModel.when AddItem {cartId, stockId, quantity = amount}
            EventModel.therefore do
              EventModel.event ItemAddedPayload {itemAddedEntityId = cartId, itemAddedStockId = stockId, itemAddedQuantity = amount}


rejectionRuleModel :: Result Text EventModel.EventModel
rejectionRuleModel = EventModel.eventModel do
  EventModel.appName "Rejection rules"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Commands" do
        EventModel.slice "Add an item" do
          EventModel.entity @CartEntity do
            EventModel.command @AddItem do
              EventModel.produces @ItemAddedPayload

          EventModel.rule "any rejection reason" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.rejected

          EventModel.rule "exact rejection reason" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.rejectedWith "Cart not found!"

          EventModel.rule "regex searches the reason" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.rejectedMatches "not found"

          EventModel.rule "wrong exact reason" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.rejectedWith "Different reason"

          EventModel.rule "wrong regex reason" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.rejectedMatches "^Missing cart$"

          EventModel.rule "rejection is not accepted without events" do
            EventModel.givenNothing
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.acceptedWithNoEvents

          EventModel.rule "emitted event is not accepted without events" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when AddItem {cartId = Uuid.nil, stockId = Uuid.nil, quantity = 1}
            EventModel.therefore do
              EventModel.acceptedWithNoEvents


viewRuleModel :: Result Text EventModel.EventModel
viewRuleModel = EventModel.eventModel do
  EventModel.appName "View action rules"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Queries" do
        EventModel.slice "Shared projection" do
          EventModel.query @RuleProjection do
            EventModel.readsFrom @CartEntity do
              EventModel.onEvent @CartCreatedPayload
              EventModel.onEvent @IBANSavedPayload
            EventModel.readsFrom @StockEntity do
              EventModel.onEvent @StockInitializedPayload

          EventModel.rule "multi-entity update uses query row ID" do
            EventModel.given do
              EventModel.event StockInitializedPayload {initializedEntityId = Uuid.nil, initializedProductId = Uuid.nil, initializedAvailable = 5}
            EventModel.when CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.therefore do
              EventModel.updatedTo fixtureRowId RuleProjection {rowId = fixtureRowId, cartSeen = True, stockSeen = True, itemCount = 0, stockCount = 1}

          EventModel.rule "delete existing projection" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "delete"}
            EventModel.therefore do
              EventModel.deleted @RuleProjection fixtureRowId

          EventModel.rule "no-op leaves projection unchanged" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "noop"}
            EventModel.therefore do
              EventModel.noOp @RuleProjection fixtureRowId

          EventModel.rule "update with unchanged row is not no-op" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "same"}
            EventModel.therefore do
              EventModel.noOp @RuleProjection fixtureRowId

          EventModel.rule "delete needs a prior row" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
              EventModel.event IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "delete"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "delete"}
            EventModel.therefore do
              EventModel.deleted @RuleProjection fixtureRowId


fixtureRowId :: Uuid
fixtureRowId = projectionId


wrongRowId :: Uuid
wrongRowId = Uuid.generateV5 Uuid.nil "wrong-row-B"


twoRowViewRuleModel :: Result Text EventModel.EventModel
twoRowViewRuleModel = EventModel.eventModel do
  EventModel.appName "Two-row view rules"
  EventModel.features do
    EventModel.feature "Cart" do
      EventModel.chapter "Queries" do
        EventModel.slice "Two rows" do
          EventModel.query @CrossEntityView do
            EventModel.readsFrom @CartEntity do
              EventModel.onEvent @CartCreatedPayload
              EventModel.onEvent @IBANSavedPayload

          EventModel.rule "updates the authored row" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
              EventModel.event CartCreatedPayload {cartCreatedEntityId = wrongRowId, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "right-row"}
            EventModel.therefore do
              EventModel.updatedTo Uuid.nil CrossEntityView {crossEntityId = Uuid.nil, crossEntityCount = 0}

          EventModel.rule "rejects an update routed to the other row" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
              EventModel.event CartCreatedPayload {cartCreatedEntityId = wrongRowId, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "wrong-row"}
            EventModel.therefore do
              EventModel.updatedTo Uuid.nil CrossEntityView {crossEntityId = Uuid.nil, crossEntityCount = 0}

          EventModel.rule "rejects a delete routed to the other row" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
              EventModel.event CartCreatedPayload {cartCreatedEntityId = wrongRowId, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "wrong-delete"}
            EventModel.therefore do
              EventModel.deleted @CrossEntityView Uuid.nil

          EventModel.rule "rejects a no-op routed to the other row" do
            EventModel.given do
              EventModel.event CartCreatedPayload {cartCreatedEntityId = Uuid.nil, cartCreatedOwnerId = "owner"}
              EventModel.event CartCreatedPayload {cartCreatedEntityId = wrongRowId, cartCreatedOwnerId = "owner"}
            EventModel.when IBANSavedPayload {ibanSavedEntityId = Uuid.nil, ibanSavedValue = "wrong-noop"}
            EventModel.therefore do
              EventModel.noOp @CrossEntityView Uuid.nil
