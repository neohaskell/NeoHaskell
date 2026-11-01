{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Generate concrete Event Model cases and run each against isolated service storage.
-- Register generators for the types requested by rules, then call 'runRules'.
module Service.EventModel.RuleRunner (
  Generators,
  emptyGenerators,
  withGenerator,
  RuleReport (..),
  declaredRules,
  specFor,
  runRules,
  reportText,
) where

import Array qualified
import Basics
import ConcurrentVar qualified
import Data.Dynamic (Dynamic)
import Data.Dynamic qualified as Dynamic
import Data.Proxy (Proxy (..))
import EventVariantOf (EventVariantOf (..))
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.Entity.Core (Entity (..), EntityOf, Event (..), EventOf)
import Service.Event qualified as Stored
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventModel qualified as EventModel
import Service.EventModel.Types (
  Chapter (..),
  CommandExpectation (..),
  EventModel (..),
  ExpectedEvent (..),
  ExpectedOutcome (..),
  Feature (..),
  HistoricalEvent (..),
  Primary,
  ReadSource,
  Rule (..),
  RuleAction (..),
  RuleCase (..),
  RuleSample (..),
  Slice (..),
  SliceContext (..),
  WhenValue (..),
 )
import Service.EventModel.Types qualified as Types
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemoryEvents
import Service.Query.Core (EntitiesOf, QueryAction (..))
import Service.Query.Definition (WireEntities (..))
import Service.Query.Definition qualified as QueryDefinition
import Service.Query.Registry (QueryRegistry, QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as InMemoryQueries
import Service.Response (CommandResponse (..))
import Service.ServiceDefinition.Core (RunnableCommand (..))
import Stream qualified
import Task (Task)
import Task qualified
import Test.Hspec qualified as Hspec
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Random qualified as Random
import Text (Text)
import Text qualified
import Text.Regex.TDFA ((=~))
import ToText (toText)
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection
import Uuid (Uuid)
import Prelude qualified


data Generator where
  Generator :: (Typeable value) => Proxy value -> Gen value -> (value -> Text) -> Generator


newtype Generators = Generators [Generator]


emptyGenerators :: Generators
emptyGenerators = Generators []


-- | Supply a generator for one requested type. Later registrations win.
withGenerator :: forall value. (Typeable value) => Gen value -> (value -> Text) -> Generators -> Generators
withGenerator generate render (Generators existing) =
  Generators (Generator (Proxy @value) generate render : existing)


data RuleReport = RuleReport
  { identity :: Text,
    sampleIndex :: Int,
    seed :: Int,
    inputs :: [Text],
    result :: Result Text ()
  }


data RulePlan where
  RulePlan :: Text -> Text -> Primary context -> Rule context -> RulePlan


-- | List each direct rule once. Translation Slices contribute no entries.
declaredRules :: EventModel -> [Text]
declaredRules model = [identity | RulePlan identity _ _ _ <- modelPlans model]


ruleIdentity :: Feature -> Chapter -> Slice -> Rule context -> Text
ruleIdentity selectedFeature selectedChapter (Slice sliceName _ _) selectedRule =
  Text.joinWith
    " / "
    (Array.fromLinkedList [selectedFeature.featureName, selectedChapter.chapterName, sliceName, selectedRule.ruleName])


-- | Run every direct rule for the requested sample count using reproducible seeds.
runRules :: Generators -> Int -> Int -> EventModel -> Task Text [RuleReport]
runRules generators baseSeed sampleCount model = do
  groups <- Prelude.mapM (runPlan generators baseSeed sampleCount) (Prelude.zip [0 ..] (modelPlans model))
  Task.yield (Prelude.concat groups)


-- | Create one Hspec example per declared direct rule.
specFor :: Generators -> Int -> Int -> EventModel -> Hspec.Spec
specFor generators baseSeed sampleCount model = do
  let plans = Prelude.zip [0 ..] (modelPlans model)
  Prelude.mapM_
    ( \plan@(_, RulePlan identity _ _ _) ->
        Hspec.it (Text.toLinkedList identity) do
          reports <- runPlan generators baseSeed sampleCount plan |> Task.runOrPanic
          let failures = [reportText report | report <- reports, case report.result of Err _ -> True; Ok () -> False]
          Hspec.shouldBe failures ([] :: [Text])
    )
    plans


modelPlans :: EventModel -> [RulePlan]
modelPlans model =
  [ RulePlan (ruleIdentity selectedFeature selectedChapter selectedSlice selectedRule) name primary selectedRule
  | selectedFeature <- model.modelFeatures,
    selectedChapter <- selectedFeature.featureChapters,
    selectedSlice@(Slice name primary rules) <- selectedChapter.chapterSlices,
    selectedRule <- rules
  ]


runPlan :: Generators -> Int -> Int -> (Int, RulePlan) -> Task Text [RuleReport]
runPlan generators baseSeed sampleCount (ruleIndex, RulePlan identity sliceName primary selectedRule) =
  if sampleCount <= 0
    then
      Task.yield
        [RuleReport {identity, sampleIndex = 0, seed = baseSeed, inputs = [], result = Err "sample count must be positive"}]
    else Prelude.mapM (runOne generators identity sliceName primary selectedRule ruleIndex baseSeed) [0 .. sampleCount - 1]


runOne :: Generators -> Text -> Text -> Primary context -> Rule context -> Int -> Int -> Int -> Task Text RuleReport
runOne generators identity sliceName primary selectedRule ruleIndex baseSeed sampleIndex = do
  let seed = baseSeed + ruleIndex * 1000003 + sampleIndex
  let generated = Gen.unGen (generateValues generators selectedRule.ruleScript) (Random.mkQCGen seed) 30
  case generated of
    Err problem -> Task.yield RuleReport {identity, sampleIndex, seed, inputs = [], result = Err problem}
    Ok (values, inputs) -> do
      let sampled = EventModel.sampleSliceRules [values] (Slice sliceName primary [selectedRule])
      case sampled of
        [Ok selectedSample] -> do
          result <- executeIsolated selectedSample |> Task.asResult
          Task.yield RuleReport {identity, sampleIndex, seed, inputs, result}
        [Err problem] -> Task.yield RuleReport {identity, sampleIndex, seed, inputs, result = Err problem}
        _ -> Task.yield RuleReport {identity, sampleIndex, seed, inputs, result = Err "rule sampling returned no case"}


generateValues :: Generators -> RuleAction context value -> Gen (Result Text ([Dynamic], [Text]))
generateValues generators action = do
  generated <- generateAction generators action
  case generated of
    Err problem -> Prelude.pure (Err problem)
    Ok (_, values, inputs) -> Prelude.pure (Ok (values, inputs))


generateAction :: Generators -> RuleAction context value -> Gen (Result Text (value, [Dynamic], [Text]))
generateAction generators action = case action of
  RuleDone value -> Prelude.pure (Ok (value, [], []))
  RuleRecord _ -> Prelude.pure (Ok ((), [], []))
  RuleRequest (_ :: Proxy requested) -> do
    result <- generateRequested @requested generators
    case result of
      Err problem -> Prelude.pure (Err problem)
      Ok ([dynamicValue], [description]) -> case Dynamic.fromDynamic @requested dynamicValue of
        Just value -> Prelude.pure (Ok (value, [dynamicValue], [description]))
        Nothing -> Prelude.pure (Err "generator returned the wrong type")
      _ -> Prelude.pure (Err "generator returned an invalid value list")
  RuleBind first next -> do
    firstResult <- generateAction generators first
    case firstResult of
      Err problem -> Prelude.pure (Err problem)
      Ok (value, firstValues, firstInputs) -> do
        secondResult <- generateAction generators (next value)
        case secondResult of
          Err problem -> Prelude.pure (Err problem)
          Ok (finalValue, secondValues, secondInputs) ->
            Prelude.pure (Ok (finalValue, firstValues Prelude.++ secondValues, firstInputs Prelude.++ secondInputs))


generateRequested ::
  forall (requested :: Type). (Typeable requested) => Generators -> Gen (Result Text ([Dynamic], [Text]))
generateRequested (Generators choices) = case findGenerator @requested choices of
  Nothing -> Prelude.pure (Err [fmt|No generator registered for #{Types.typeName (Proxy @requested)}|])
  Just (Generator (_ :: Proxy supplied) generate render) -> do
    value <- generate
    case Dynamic.fromDynamic @requested (Dynamic.toDyn value) of
      Nothing -> Prelude.pure (Err "generator registry type mismatch")
      Just requestedValue ->
        Prelude.pure
          (Ok ([Dynamic.toDyn requestedValue], [[fmt|#{Types.typeName (Proxy @requested)}=#{render value}|]]))


findGenerator :: forall (requested :: Type). (Typeable requested) => [Generator] -> Maybe Generator
findGenerator choices = case choices of
  [] -> Nothing
  selected@(Generator (_ :: Proxy supplied) _ _) : rest ->
    if Reflection.SomeTypeRep (Reflection.typeRep @requested) == Reflection.SomeTypeRep (Reflection.typeRep @supplied)
      then Just selected
      else findGenerator @requested rest


executeIsolated :: RuleSample -> Task Text ()
executeIsolated selected = do
  store <- InMemoryEvents.new |> Task.mapError toText
  Task.finally store.close (executeSample store selected)


executeSample :: EventStore Json.Value -> RuleSample -> Task Text ()
executeSample store selected = case selected of
  ChangeSample (_ :: Proxy entity) (_ :: Proxy command) _ ruleCase ->
    executeChange @entity @command store ruleCase
  ViewSample (_ :: Proxy query) sources _ ruleCase ->
    executeView @query store sources ruleCase


executeChange ::
  forall entity command.
  (RunnableCommand command, EntityOf command ~ entity, Json.ToJSON (EventOf entity)) =>
  EventStore Json.Value -> RuleCase ('Change entity command) -> Task Text ()
executeChange store selected = do
  Prelude.mapM_ (seedHistory store Nothing) selected.ruleHistory
  earlier <- allEvents store
  response <- case selected.ruleWhen of
    CommandWhen value -> runRuleCommand @command store value
  later <- allEvents store
  let emitted = Prelude.map (\item -> item.event) (Prelude.drop (Prelude.length earlier) later)
  case selected.ruleExpected of
    CommandExpected expected -> checkCommand expected response emitted


checkCommand ::
  forall entity.
  (Json.ToJSON (EventOf entity)) => CommandExpectation entity -> CommandResponse -> [Json.Value] -> Task Text ()
checkCommand expected response emitted = case response of
  Accepted {} -> case expected of
    Emitted events -> do
      let wanted = [Json.toJSON (fromVariant value :: EventOf entity) | ExpectedEvent value <- events]
      Task.unless (wanted == emitted) do
        Task.throw [fmt|ordered events differ; expected #{showText wanted}; actual #{showText emitted}|]
    AcceptedWithoutEvents ->
      Task.unless (Prelude.null emitted) do
        Task.throw [fmt|expected accepted with no events; actual #{showText emitted}|]
    RejectedReason _ -> Task.throw [fmt|expected rejection; actual accepted with #{showText emitted}|]
    RejectedPattern _ -> Task.throw [fmt|expected rejection; actual accepted with #{showText emitted}|]
  Rejected reason -> case expected of
    RejectedReason Nothing -> Task.yield unit
    RejectedReason (Just wanted) ->
      Task.unless (wanted == reason) do
        Task.throw [fmt|rejection reason differs; expected #{wanted}; actual #{reason}|]
    RejectedPattern patternText ->
      Task.unless (Text.toLinkedList reason =~ Text.toLinkedList patternText) do
        Task.throw [fmt|rejection pattern did not match; pattern #{patternText}; actual #{reason}|]
    Emitted _ -> Task.throw [fmt|expected ordered events; actual rejection #{reason}|]
    AcceptedWithoutEvents -> Task.throw [fmt|expected accepted with no events; actual rejection #{reason}|]
  Failed problem -> Task.throw [fmt|framework error: #{problem}|]
  Unauthorized problem -> Task.throw [fmt|framework authorization error: #{toText problem}|]


allEvents :: EventStore Json.Value -> Task Text [Stored.Event Json.Value]
allEvents store = do
  stream <-
    store.readAllEventsForwardFrom (Stored.StreamPosition 0) (EventStore.Limit 9223372036854775807)
      |> Task.mapError toText
  messages <- Stream.toArray stream
  Task.yield (messages |> EventStore.collectAllEvents |> Array.toLinkedList)


seedHistory :: EventStore Json.Value -> Maybe QueryRegistry -> HistoricalEvent -> Task Text ()
seedHistory store maybeRegistry (HistoricalEvent (_ :: Proxy entity) value) = do
  rawEvent <- insertConcrete @entity store value
  case maybeRegistry of
    Nothing -> Task.yield unit
    Just registry -> updateProjection registry rawEvent


insertConcrete ::
  forall entity payload.
  ( Entity entity,
    Event (EventOf entity),
    EntityOf (EventOf entity) ~ entity,
    EventVariantOf (EventOf entity) payload,
    StreamId.ToStreamId (EntityIdType entity),
    GHC.KnownSymbol (NameOf entity),
    Json.ToJSON (EventOf entity)
  ) =>
  EventStore Json.Value -> payload -> Task Text (Stored.Event Json.Value)
insertConcrete store concrete = do
  let eventValue = fromVariant concrete :: EventOf entity
  let entityName = EntityName (GHC.symbolVal (Proxy @(NameOf entity)) |> Text.fromLinkedList)
  let streamId = StreamId.toStreamId (getEventEntityIdImpl eventValue)
  let rawValue = Json.toJSON eventValue
  payload <- Stored.payloadFromEvents entityName streamId (Array.wrap rawValue)
  _ <- store.insert payload |> Task.mapError toText
  case Array.first payload.insertions of
    Nothing -> Task.throw "event insertion did not contain the authored fixture"
    Just insertion -> Task.yield Stored.Event {entityName, streamId, event = rawValue, metadata = insertion.metadata}


updateProjection :: QueryRegistry -> Stored.Event Json.Value -> Task Text ()
updateProjection registry rawEvent = do
  let updaters = Registry.getUpdatersForEntity rawEvent.entityName registry
  _ <- updaters |> Task.mapArray (\updater -> updater.updateQuery rawEvent)
  Task.yield unit


executeView ::
  forall query.
  ( Json.ToJSON query,
    GHC.KnownSymbol (NameOf query),
    WireEntities (EntitiesOf query) query
  ) =>
  EventStore Json.Value -> [ReadSource query] -> RuleCase ('View query) -> Task Text ()
executeView store sources selected = do
  underlying <- InMemoryQueries.new @query |> Task.mapError toText
  rows <- ConcurrentVar.containing (Map.empty :: Map Uuid query)
  observed <- ConcurrentVar.containing (Nothing :: Maybe (Uuid, QueryAction query))
  let tracked =
        underlying
          { atomicUpdate = \rowId update -> do
              underlying.atomicUpdate rowId update
              current <- underlying.get rowId
              rows
                |> ConcurrentVar.modify
                  ( \snapshot -> case current of
                      Nothing -> Map.remove rowId snapshot
                      Just value -> Map.set rowId value snapshot
                  )
          }
  let queryName = GHC.symbolVal (Proxy @(NameOf query)) |> Text.fromLinkedList
  registry <-
    wireEntitiesWithObserver @(EntitiesOf query) @query
      queryName
      store
      tracked
      (Just (\rowId action -> observed |> ConcurrentVar.modify (\_ -> Just (rowId, action))))
  let subscriptions = Prelude.map EventModel.sourceSubscription sources
  let selectedRegistry = QueryDefinition.restrictRegistryToSources subscriptions registry
  Prelude.mapM_ (seedHistory store (Just selectedRegistry)) selected.ruleHistory
  before <- ConcurrentVar.peek rows
  _ <-
    ( case selected.ruleWhen of
        EventWhen (value :: payload) -> do
          rawEvent <- insertConcrete @(EntityOf payload) store value
          updateProjection selectedRegistry rawEvent
    ) ::
      Task Text ()
  after <- ConcurrentVar.peek rows
  actual <- ConcurrentVar.peek observed
  case selected.ruleExpected of
    ViewExpected rowId expected -> checkView rowId expected before after actual


checkView ::
  forall query.
  (Json.ToJSON query) =>
  Uuid -> QueryAction query -> Map Uuid query -> Map Uuid query -> Maybe (Uuid, QueryAction query) -> Task Text ()
checkView expectedRowId expected before after actual = case actual of
  Nothing ->
    Task.throw [fmt|query subscription produced no QueryAction; before #{snapshotText before}; after #{snapshotText after}|]
  Just (rowId, observed) -> do
    let actionMatches = case (expected, observed) of
          (Update wanted, Update found) -> Json.toJSON wanted == Json.toJSON found
          (Delete, Delete) -> True
          (NoOp, NoOp) -> True
          _ -> False
    let expectedAfter = case expected of
          Update wanted -> Just (Map.set expectedRowId wanted before)
          Delete -> case Map.get expectedRowId before of
            Nothing -> Nothing
            Just _ -> Just (Map.remove expectedRowId before)
          NoOp -> Just before
    let diffMatches = case expectedAfter of
          Nothing -> False
          Just wanted -> snapshotJson wanted == snapshotJson after
    Task.unless (rowId == expectedRowId && actionMatches && diffMatches) do
      Task.throw
        [fmt|query action or complete diff differs; expected row #{expectedRowId}; actual row #{rowId}; expected action #{actionText expected}; actual action #{actionText observed}; before #{snapshotText before}; after #{snapshotText after}; expected after #{showText (Prelude.fmap snapshotText expectedAfter)}|]


snapshotJson :: (Json.ToJSON query) => Map Uuid query -> Map Uuid Json.Value
snapshotJson rows = rows |> Map.mapValues Json.toJSON


snapshotText :: (Json.ToJSON query) => Map Uuid query -> Text
snapshotText rows = showText (snapshotJson rows)


actionText :: (Json.ToJSON query) => QueryAction query -> Text
actionText action = case action of
  Update value -> [fmt|Update #{Json.encodeText value}|]
  Delete -> "Delete"
  NoOp -> "NoOp"


showText :: (Show value) => value -> Text
showText value = value |> Prelude.show |> Text.fromLinkedList


reportText :: RuleReport -> Text
reportText report = do
  let outcome = case report.result of
        Ok () -> "PASS"
        Err problem -> [fmt|FAIL #{problem}|] :: Text
  let identity = report.identity
  let sampleIndex = report.sampleIndex
  let seed = report.seed
  let inputs = showText report.inputs
  [fmt|#{identity} sample=#{sampleIndex} seed=#{seed} inputs=#{inputs} #{outcome}|]
