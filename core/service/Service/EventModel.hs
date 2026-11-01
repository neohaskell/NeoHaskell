{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Build a named Event Model from source-local Features.
-- Export Feature actions from their owning modules and compose them in 'features'.
module Service.EventModel (
  EventModel,
  Feature,
  Chapter,
  Slice,
  FeatureAction,
  EventModelAction,
  modelName,
  modelFeatures,
  featureName,
  featureChapters,
  chapterName,
  chapterSlices,
  sliceName,
  sliceKind,
  sliceEventTypes,
  sliceCommandTargets,
  sliceEventSources,
  sliceRuleNames,
  sampleSliceRules,
  sampleRuleName,
  sampleRuleHistory,
  sampleRuleWhen,
  sampleRuleOutcome,
  expectedEventValue,
  verifyViewObservation,
  RuleSample (..),
  RuleAction,
  SliceContext (..),
  RuleCase (..),
  HistoricalEvent (..),
  WhenValue (..),
  ExpectedOutcome (..),
  CommandExpectation (..),
  ExpectedEvent (..),
  QuerySnapshot (..),
  QueryDiff (..),
  ViewObservation (..),
  eventModel,
  toApplication,
  run,
  appName,
  withConfig,
  withApplicationConfiguration,
  withIntegration,
  withSecretStore,
  withAuthOverrides,
  withCors,
  withHealthCheck,
  withoutHealthCheck,
  useReadinessEndpoint,
  withEventStore,
  withQueryObjectStore,
  withTransport,
  withApiInfo,
  withFileUpload,
  withAuth,
  withOAuth2StateKey,
  withOAuth2Provider,
  withDispatcherConfig,
  outboundLifecycle,
  features,
  feature,
  chapter,
  slice,
  entity,
  command,
  produces,
  query,
  readsFrom,
  onEvent,
  outboundIntegration,
  inboundIntegration,
  triggersCommand,
  onTargetEntity,
  rule,
  requestValue,
  given,
  givenNothing,
  when,
  therefore,
  event,
  rejected,
  rejectedWith,
  rejectedMatches,
  regex,
  acceptedWithNoEvents,
  updatedTo,
  deleted,
  noOp,
) where

import Array qualified
import Auth.OAuth2.Provider (OAuth2ProviderConfig)
import Auth.Config (AuthOverrides)
import Auth.SecretStore (SecretStore)
import Basics
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer
import Data.Dynamic (Dynamic)
import Data.Dynamic qualified as Dynamic
import Data.Proxy (Proxy (..))
import Default (Default)
import EventVariantOf (EventVariantOf (..))
import GHC.TypeLits qualified as GHC
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Map qualified
import Maybe (Maybe (..))
import OptEnvConf (HasParser)
import Record qualified
import Result (Result (..))
import Schema qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.Application.Types (ApiInfo)
import Service.Command.Core (Command, NameOf)
import Service.Entity.Core (Entity, EntityOf, EventOf)
import Service.Event.EntityName (EntityName (..))
import Service.EventModel.Types (
  Chapter (..),
  CommandExpectation (..),
  CommandRoot (..),
  CommandTarget (..),
  CommandTargetFor (..),
  EventModel (..),
  EventRef (..),
  ExpectedEvent (..),
  ExpectedOutcome (..),
  Feature (..),
  HistoricalEvent (..),
  InboundPart (..),
  ModelPart (..),
  OutboundPart (..),
  OutcomePart (..),
  Primary (..),
  Produced (..),
  QueryDiff (..),
  QuerySnapshot (..),
  QuerySourceMember,
  ReadSource (..),
  Rule (..),
  RuleAction (..),
  RuleCase (..),
  RulePart (..),
  RuleSample (..),
  Slice (..),
  SliceContext (..),
  SlicePart (..),
  ViewObservation (..),
  WhenValue (..),
 )
import Service.EventModel.Types qualified as Types
import Service.EventStore (EventStoreConfig)
import Service.FileUpload.Core (FileUploadConfig)
import Service.Integration.Adapter (Integration)
import Service.Integration.Adapter qualified as Adapter
import Service.InboundIntegration.Core (InboundIntegration (..))
import Service.Integration.Dispatcher (DispatcherConfig)
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Query.Core (EntitiesOf, Query, QueryAction (..), QueryOf)
import Service.Query.Definition (EventSubscription (..), WireEntities)
import Service.QueryObjectStore.Core (QueryObjectStoreConfig)
import Service.ServiceDefinition.Core (RunnableCommand (..))
import Service.Transport (Transport)
import Service.Transport.Web qualified as Web
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection
import TypeName qualified
import Uuid (Uuid)
import Prelude qualified


-- | Export one of these actions from a component's Feature module.
type FeatureAction = Writer [([Text], Feature)] ()


type EventModelAction = Writer [ModelPart] ()


modelName :: EventModel -> Text
modelName model = model.modelName


modelFeatures :: EventModel -> [Feature]
modelFeatures model = model.modelFeatures


featureName :: Feature -> Text
featureName selected = selected.featureName


featureChapters :: Feature -> [Chapter]
featureChapters selected = selected.featureChapters


chapterName :: Chapter -> Text
chapterName selected = selected.chapterName


chapterSlices :: Chapter -> [Slice]
chapterSlices selected = selected.chapterSlices


sliceName :: Slice -> Text
sliceName (Slice name _ _) = name


sliceRuleNames :: Slice -> [Text]
sliceRuleNames (Slice _ _ rules) = [selected.ruleName | selected <- rules]


-- | Supply one ordered value list per rule and materialize each case.
-- The test runner owns how those values are generated and isolated.
sampleSliceRules :: [[Dynamic]] -> Slice -> [Result Text RuleSample]
sampleSliceRules valueLists (Slice name primary rules) =
  if Prelude.length valueLists Prelude./= Prelude.length rules
    then [Err [fmt|Slice "#{name}" requires one value list per rule|]]
    else sampleRules primary (Prelude.zip valueLists rules)


sampleRules :: Primary context -> [([Dynamic], Rule context)] -> [Result Text RuleSample]
sampleRules primary suppliedRules = case primary of
  StateChange entityProxy (CommandRoot commandProxy _ _) ->
    [ do
        selectedCase <- sampleRule values selected
        Ok (ChangeSample entityProxy commandProxy selected.ruleName selectedCase)
    | (values, selected) <- suppliedRules
    ]
  StateView queryProxy sources _ ->
    [ do
        selectedCase <- sampleRule values selected
        validateViewSubscription selected.ruleName sources selectedCase
        Ok (ViewSample queryProxy selected.ruleName selectedCase)
    | (values, selected) <- suppliedRules
    ]
  OutboundTranslation _ _ _ -> []
  InboundTranslation _ _ _ -> []


sampleRule :: [Dynamic] -> Rule context -> Result Text (RuleCase context)
sampleRule values selected = do
  (_, unused, parts) <- runRuleAction values selected.ruleScript
  let name = selected.ruleName
  if Prelude.null unused
    then Ok ()
    else Err [fmt|Rule "#{name}" received unused values|]
  ruleCase name parts


runRuleAction ::
  forall context value. [Dynamic] -> RuleAction context value -> Result Text (value, [Dynamic], [RulePart context])
runRuleAction values action = case action of
  RuleDone value -> Ok (value, values, [])
  RuleBind first next -> do
    (intermediate, remaining, firstParts) <- runRuleAction values first
    (result, finalValues, nextParts) <- runRuleAction remaining (next intermediate)
    Ok (result, finalValues, firstParts Prelude.++ nextParts)
  RuleRequest (proxy :: Proxy requested) -> do
    let requestedName = Types.typeName proxy
    case values of
      [] -> Err [fmt|Rule value request for #{requestedName} has no supplied value|]
      supplied : rest -> case Dynamic.fromDynamic @requested supplied of
        Just value -> Ok (value, rest, [])
        Nothing -> Err [fmt|Rule value request for #{requestedName} received another type|]
  RuleRecord part -> Ok ((), values, [part])


sampleRuleName :: RuleSample -> Text
sampleRuleName sample = case sample of
  ChangeSample _ _ name _ -> name
  ViewSample _ name _ -> name


sampleRuleHistory :: RuleSample -> [Dynamic]
sampleRuleHistory sample = case sample of
  ChangeSample _ _ _ selected -> [Dynamic.toDyn payload | HistoricalEvent _ payload <- selected.ruleHistory]
  ViewSample _ _ selected -> [Dynamic.toDyn payload | HistoricalEvent _ payload <- selected.ruleHistory]


sampleRuleWhen :: RuleSample -> Dynamic
sampleRuleWhen sample = case sample of
  ChangeSample _ _ _ selected -> case selected.ruleWhen of
    CommandWhen value -> Dynamic.toDyn value
  ViewSample _ _ selected -> case selected.ruleWhen of
    EventWhen value -> Dynamic.toDyn value


sampleRuleOutcome :: RuleSample -> Dynamic
sampleRuleOutcome sample = case sample of
  ChangeSample _ _ _ selected -> case selected.ruleExpected of
    CommandExpected expected -> Dynamic.toDyn expected
  ViewSample _ _ selected -> case selected.ruleExpected of
    ViewExpected expected -> Dynamic.toDyn expected


expectedEventValue :: ExpectedEvent entity -> Dynamic
expectedEventValue (ExpectedEvent value) = Dynamic.toDyn value


-- | Compare the action and every row before and after one query update.
-- The caller supplies the query row ID; it need not match the event entity ID.
verifyViewObservation :: forall query. (Eq query) => Uuid -> QueryAction query -> ViewObservation query -> Bool
verifyViewObservation queryId expected observed = do
  let QuerySnapshot before = observed.diff.before
  let QuerySnapshot after = observed.diff.after
  let expectedDiff = case expected of
        Update value -> after == Map.set queryId value before
        Delete -> case Map.get queryId before of
          Just _ -> after == Map.remove queryId before
          Nothing -> False
        NoOp -> after == before
  observed.action == expected && expectedDiff


validateViewSubscription :: Text -> [ReadSource query] -> RuleCase ('View query) -> Result Text ()
validateViewSubscription name sources selected = case selected.ruleWhen of
  EventWhen (_ :: payload) -> do
    let declared =
          [ Reflection.SomeTypeRep (Reflection.typeRep @eventPayload)
          | ReadSource _ events <- sources,
            EventRef (_ :: Proxy eventPayload) <- events
          ]
    let incoming = Reflection.SomeTypeRep (Reflection.typeRep @payload)
    if Prelude.elem incoming declared
      then Ok ()
      else
        Err
          [fmt|Rule "#{name}" when payload #{Types.typeName (Proxy @payload)} is not listed by the query's onEvent subscriptions|]


-- | Assemble a named topology and reject malformed local structure.
-- Evaluation does not derive Event ADTs or start application workers.
eventModel :: EventModelAction -> Result Text EventModel
eventModel builder =
  let parts = Writer.execWriter builder
      names = [name | ModelName name <- parts]
      featureResults = Prelude.concat [found | ModelFeatures found <- parts]
      configRegistrations = [configure | ModelConfigRegistration configure <- parts]
      configuration = configRegistrations Prelude.++ [configure | ModelConfiguration configure <- parts]
      configProblems =
        if Prelude.length configRegistrations > 1
          then ["eventModel allows at most one withConfig"]
          else []
      featureProblems = [problem | (problems, _) <- featureResults, problem <- problems]
      validFeatures = [selected | (_, selected) <- featureResults]
  in  case names of
        [name] ->
          let model = EventModel {modelName = name, modelFeatures = validFeatures, modelConfiguration = configuration}
              modelProblems = case validateModel model of
                Ok _ -> []
                Err problem -> [problem]
              problems = featureProblems Prelude.++ configProblems Prelude.++ modelProblems
          in  case problems of
                [] -> Ok model
                _ -> Err (Text.joinWith "\n" (Array.fromLinkedList problems))
        _ -> Err (Text.joinWith "\n" (Array.fromLinkedList ("eventModel requires exactly one appName" : featureProblems Prelude.++ configProblems)))


-- | Register every validated Slice with the existing Application runtime.
toApplication :: EventModel -> Application
toApplication model = do
  let configured = Prelude.foldl (\application configure -> configure application) Application.new model.modelConfiguration
  Prelude.foldl registerFeature configured model.modelFeatures


-- | Validate the named model before starting application effects.
run :: Result Text EventModel -> Task Text Unit
run modelResult = case modelResult of
  Err problem -> Task.throw problem
  Ok model -> Application.run (toApplication model)


withConfig :: forall config. (HasParser config, Typeable config) => EventModelAction
withConfig = Writer.tell [ModelConfigRegistration (Application.withConfig @config)]


-- | Add an Application configuration operation inside the named model.
-- Register the application's config type with 'withConfig' so duplicates fail preflight.
withApplicationConfiguration :: (Application -> Application) -> EventModelAction
withApplicationConfiguration configure = Writer.tell [ModelConfiguration configure]


withIntegration ::
  forall request.
  ( Integration request,
    Typeable request,
    Typeable (Adapter.Response request),
    TypeName.Inspectable request
  ) =>
  EventModelAction
withIntegration = withApplicationConfiguration (Application.withIntegration @request)


withSecretStore :: forall config. (Typeable config) => (config -> SecretStore) -> EventModelAction
withSecretStore makeStore = withApplicationConfiguration (Application.withSecretStore makeStore)


withAuthOverrides :: forall config. (Typeable config) => (config -> Text) -> AuthOverrides -> EventModelAction
withAuthOverrides makeUrl overrides = withApplicationConfiguration (Application.withAuthOverrides makeUrl overrides)


withCors :: forall config. (Typeable config) => (config -> Web.CorsConfig) -> EventModelAction
withCors makeConfig = withApplicationConfiguration (Application.withCors makeConfig)


withHealthCheck :: forall config. (Typeable config) => (config -> Text) -> EventModelAction
withHealthCheck makePath = withApplicationConfiguration (Application.withHealthCheck makePath)


withoutHealthCheck :: EventModelAction
withoutHealthCheck = withApplicationConfiguration Application.withoutHealthCheck


useReadinessEndpoint :: EventModelAction
useReadinessEndpoint = withApplicationConfiguration Application.useReadinessEndpoint


withEventStore ::
  forall config eventStoreConfig.
  (Typeable config, EventStoreConfig eventStoreConfig) => (config -> eventStoreConfig) -> EventModelAction
withEventStore makeConfig = Writer.tell [ModelConfiguration (Application.withEventStore makeConfig)]


withQueryObjectStore :: forall config. (QueryObjectStoreConfig config) => config -> EventModelAction
withQueryObjectStore config = Writer.tell [ModelConfiguration (Application.withQueryObjectStore config)]


withTransport ::
  forall transport transportName.
  (Transport transport, transportName ~ NameOf transport, Record.KnownHash transportName, GHC.KnownSymbol transportName) =>
  transport -> EventModelAction
withTransport transport = Writer.tell [ModelConfiguration (Application.withTransport transport)]


withApiInfo :: forall config. (Typeable config) => (config -> ApiInfo) -> EventModelAction
withApiInfo makeInfo = Writer.tell [ModelConfiguration (Application.withApiInfo makeInfo)]


withFileUpload :: forall config. (Typeable config) => (config -> FileUploadConfig) -> EventModelAction
withFileUpload makeConfig = Writer.tell [ModelConfiguration (Application.withFileUpload makeConfig)]


withAuth :: forall config. (Typeable config) => (config -> Text) -> EventModelAction
withAuth makeUrl = Writer.tell [ModelConfiguration (Application.withAuth makeUrl)]


withOAuth2StateKey :: Text -> EventModelAction
withOAuth2StateKey name = Writer.tell [ModelConfiguration (Application.withOAuth2StateKey name)]


withOAuth2Provider :: forall config. (Typeable config) => (config -> OAuth2ProviderConfig) -> EventModelAction
withOAuth2Provider makeConfig = Writer.tell [ModelConfiguration (Application.withOAuth2Provider makeConfig)]


withDispatcherConfig :: forall config. (Typeable config) => (config -> DispatcherConfig) -> EventModelAction
withDispatcherConfig makeConfig = Writer.tell [ModelConfiguration (Application.withDispatcherConfig makeConfig)]


registerFeature :: Application -> Feature -> Application
registerFeature application selected = Prelude.foldl registerChapter application selected.featureChapters


registerChapter :: Application -> Chapter -> Application
registerChapter application selected = Prelude.foldl registerSlice application selected.chapterSlices


registerSlice :: Application -> Slice -> Application
registerSlice application (Slice _ primary _) = case primary of
  StateChange _ (CommandRoot _ _ runner) -> Application.withServiceRunner runner application
  StateView _ _ register -> register application
  OutboundTranslation _ _ register -> register application
  InboundTranslation _ _ register -> register application


appName :: Text -> EventModelAction
appName name = Writer.tell [ModelName name]


features :: FeatureAction -> EventModelAction
features builder = Writer.tell [ModelFeatures (Writer.execWriter builder)]


feature :: Text -> Writer [([Text], Chapter)] () -> FeatureAction
feature name builder = do
  let found = Writer.execWriter builder
  let problems = [[fmt|Feature "#{name}", #{problem}|] | (chapterProblems, _) <- found, problem <- chapterProblems]
  let empty = if Prelude.null found then [[fmt|Feature "#{name}" requires a Chapter|]] else []
  let chapters = [selected | (_, selected) <- found]
  Writer.tell [(empty Prelude.++ problems, Feature {featureName = name, featureChapters = chapters})]


chapter :: Text -> Writer [Result Text Slice] () -> Writer [([Text], Chapter)] ()
chapter name builder = do
  let found = Writer.execWriter builder
  let (sliceProblems, slices) = collectResults found
  let problems = [[fmt|Chapter "#{name}", #{problem}|] | problem <- sliceProblems]
  let empty = if Prelude.null found then [[fmt|Chapter "#{name}" requires a Slice|]] else []
  Writer.tell [(empty Prelude.++ problems, Chapter {chapterName = name, chapterSlices = slices})]


collectResults :: [Result Text value] -> ([Text], [value])
collectResults results =
  let failures = [problem | Err problem <- results]
      values = [value | Ok value <- results]
  in  (failures, values)


-- | Add one inline Slice with exactly one primary declaration.
-- The model entry point reports a named error if the block has zero or several primaries.
slice :: Text -> Writer [SlicePart context] () -> Writer [Result Text Slice] ()
slice name builder = do
  let parts = Writer.execWriter builder
  let primaries = [primary | SlicePrimary primary <- parts]
  let rules = [selected | SliceRule selected <- parts]
  let primaryCount = Prelude.length primaries
  let result = case primaries of
        [primary] -> Ok (Slice name primary rules)
        _ ->
          Err
            [fmt|Slice "#{name}" requires exactly one primary; found #{primaryCount}: #{Text.joinWith ", " (Array.fromLinkedList (Prelude.map Types.primaryKind primaries))}|]
  Writer.tell [result]


entity ::
  forall entity command.
  (Entity entity, Typeable entity) => CommandRoot entity command -> Writer [SlicePart ('Change entity command)] ()
entity root = Writer.tell [SlicePrimary (StateChange (Proxy @entity) root)]


command ::
  forall command entity.
  (RunnableCommand command, EntityOf command ~ entity, Typeable command) =>
  Writer [Produced entity] () -> CommandRoot entity command
command builder = CommandRoot (Proxy @command) (Writer.execWriter builder) (commandRunner @command)


produces ::
  forall payload entity.
  (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => Writer [Produced entity] ()
produces = Writer.tell [Produced (Proxy @payload)]


query ::
  forall query queryName entities.
  ( Query query,
    Typeable query,
    Schema.ToSchema query,
    Json.ToJSON query,
    Json.FromJSON query,
    queryName ~ NameOf query,
    entities ~ EntitiesOf query,
    GHC.KnownSymbol queryName,
    WireEntities entities query
  ) =>
  Writer [ReadSource query] () -> Writer [SlicePart ('View query)] ()
query builder = do
  let sources = Writer.execWriter builder
  let subscriptions = Prelude.map sourceSubscription sources
  Writer.tell [SlicePrimary (StateView (Proxy @query) sources (Application.withQuerySources @query subscriptions))]


readsFrom ::
  forall entity query.
  ( Entity entity,
    QueryOf entity query,
    QuerySourceMember entity query (EntitiesOf query),
    Typeable entity,
    GHC.KnownSymbol (NameOf entity),
    Json.FromJSON (EventOf entity)
  ) =>
  Writer [EventRef entity] () -> Writer [ReadSource query] ()
readsFrom builder = Writer.tell [ReadSource (Proxy @entity) (Writer.execWriter builder)]


sourceSubscription :: ReadSource query -> EventSubscription
sourceSubscription (ReadSource (_ :: Proxy entity) events) = do
  let name = GHC.symbolVal (Proxy @(NameOf entity)) |> Text.fromLinkedList
  EventSubscription (EntityName name) (matchesEvent @entity events)


matchesEvent :: forall entity. (Json.FromJSON (EventOf entity)) => [EventRef entity] -> Json.Value -> Bool
matchesEvent events rawEvent = case Json.decode @(EventOf entity) rawEvent of
  Err _ -> False
  Ok decoded -> Prelude.any (matchesPayload decoded) events


matchesPayload :: forall entity. EventOf entity -> EventRef entity -> Bool
matchesPayload decoded (EventRef (_ :: Proxy payload)) = case toVariant @(EventOf entity) @payload decoded of
  Just _ -> True
  Nothing -> False


class EventPart part payload where
  makeEventPart :: Proxy payload -> part


instance
  (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) =>
  EventPart (EventRef entity) payload
  where
  makeEventPart _ = EventRef (Proxy @payload)


instance
  ( EntityOf payload ~ EntityOf handler,
    HandledEvent handler ~ payload,
    EventVariantOf (EventOf (EntityOf handler)) payload,
    Typeable payload
  ) =>
  EventPart (OutboundPart handler) payload
  where
  makeEventPart _ = OutboundEvent (Proxy @payload)


-- | Name a concrete payload subscription in a query source or outbound handler.
onEvent :: forall payload part. (EventPart part payload) => Writer [part] ()
onEvent = Writer.tell [makeEventPart @part @payload Proxy]


outboundIntegration ::
  forall handler entity event.
  ( OutboundIntegration handler,
    Typeable handler,
    entity ~ EntityOf handler,
    event ~ EventOf entity,
    EventVariantOf event (HandledEvent handler),
    Json.FromJSON entity,
    Json.FromJSON event,
    TypeName.Inspectable entity,
    Default entity,
    Entity entity
  ) =>
  Writer [OutboundPart handler] () -> Writer [SlicePart ('Outbound handler)] ()
outboundIntegration builder = do
  let parts = Writer.execWriter builder
  let register application = Prelude.foldl registerLifecycle (Application.withOutbound @handler application) parts
  Writer.tell [SlicePrimary (OutboundTranslation (Proxy @handler) parts register)]


registerLifecycle :: Application -> OutboundPart handler -> Application
registerLifecycle application part = case part of
  OutboundLifecycle register -> register application
  OutboundEvent _ -> application
  OutboundCommand _ -> application


-- | Attach a configured lifecycle worker to the owning outbound Slice.
outboundLifecycle ::
  forall config (entity :: Type) state handler.
  (Typeable config, entity ~ EntityOf handler, TypeName.Inspectable entity) =>
  (config -> Lifecycle.OutboundConfig state) -> Writer [OutboundPart handler] ()
outboundLifecycle makeConfig = Writer.tell [OutboundLifecycle (Application.withOutboundLifecycle @config @entity makeConfig)]


inboundIntegration ::
  forall handler.
  ( InboundIntegration handler,
    Types.KnownInboundTargets (InboundOutputsOf handler),
    Typeable handler,
    Typeable (InboundConfigOf handler)
  ) =>
  Writer [InboundPart handler] () -> Writer [SlicePart ('Inbound handler)] ()
inboundIntegration builder =
  Writer.tell
    [ SlicePrimary (InboundTranslation (Proxy @handler) (Writer.execWriter builder) (Application.withInboundHandler @handler))
    ]


class CommandPart part command where
  makeCommandPart :: Proxy command -> CommandTarget -> part


instance CommandPart (OutboundPart handler) command where
  makeCommandPart _ target = OutboundCommand target


instance
  (InboundIntegration handler, Integration.AllowedOutput command (InboundOutputsOf handler)) =>
  CommandPart (InboundPart handler) command
  where
  makeCommandPart _ target = InboundCommand target


triggersCommand :: forall command part. (CommandPart part command) => CommandTargetFor command -> Writer [part] ()
triggersCommand (CommandTargetFor (entityProxy :: Proxy entity)) = Writer.tell [makeCommandPart @part @command (Proxy @command) (CommandTarget (Proxy @command) entityProxy)]


onTargetEntity ::
  forall entity command.
  (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) =>
  CommandTargetFor command
onTargetEntity = CommandTargetFor (Proxy @entity)


sliceKind :: Slice -> Text
sliceKind (Slice _ primary _) = Types.primaryKind primary


sliceEventTypes :: Slice -> [Text]
sliceEventTypes (Slice _ primary _) = case primary of
  StateChange _ (CommandRoot _ produced _) -> [Types.typeName payload | Produced payload <- produced]
  StateView _ sources _ -> Prelude.concat [eventNames events | ReadSource _ events <- sources]
  OutboundTranslation _ parts _ -> [Types.typeName payload | OutboundEvent payload <- parts]
  InboundTranslation _ _ _ -> []


sliceCommandTargets :: Slice -> [(Text, Text)]
sliceCommandTargets (Slice _ primary _) = case primary of
  StateChange entityProxy (CommandRoot commandProxy _ _) -> [(Types.typeName commandProxy, Types.typeName entityProxy)]
  StateView _ _ _ -> []
  OutboundTranslation _ parts _ -> [targetNames target | OutboundCommand target <- parts]
  InboundTranslation _ parts _ -> [targetNames target | InboundCommand target <- parts]


sliceEventSources :: Slice -> [(Text, [Text])]
sliceEventSources (Slice _ primary _) = case primary of
  StateView _ sources _ -> [(Types.typeName entityProxy, eventNames events) | ReadSource entityProxy events <- sources]
  _ -> []


eventNames :: [EventRef entity] -> [Text]
eventNames events = [Types.typeName payload | EventRef payload <- events]


targetNames :: CommandTarget -> (Text, Text)
targetNames (CommandTarget commandProxy entityProxy) = (Types.typeName commandProxy, Types.typeName entityProxy)


validateModel :: EventModel -> Result Text EventModel
validateModel model =
  let featuresFound = model.modelFeatures
      emptyModel = if Prelude.null featuresFound then ["eventModel requires at least one Feature"] else []
      paths =
        [ (selected.featureName, chapter.chapterName, sliceValue)
        | selected <- featuresFound,
          chapter <- selected.featureChapters,
          sliceValue <- chapter.chapterSlices
        ]
      (sliceProblems, _) = Prelude.foldl validatePath ([], []) paths
      problems = emptyModel Prelude.++ sliceProblems
  in  case problems of
        [] -> Ok model
        _ -> Err (Text.joinWith "\n" (Array.fromLinkedList problems))


validatePath ::
  ([Text], [(Reflection.SomeTypeRep, Text)]) -> (Text, Text, Slice) -> ([Text], [(Reflection.SomeTypeRep, Text)])
validatePath (problems, seen) (featureLabel, chapterLabel, selected) =
  let selectedName = sliceName selected
      path = [fmt|Feature "#{featureLabel}", Chapter "#{chapterLabel}", Slice "#{selectedName}"|]
      (found, next) = validateSlice path selected seen
  in  (problems Prelude.++ found, next)


validateSlice :: Text -> Slice -> [(Reflection.SomeTypeRep, Text)] -> ([Text], [(Reflection.SomeTypeRep, Text)])
validateSlice path (Slice _ primary _) seen = case primary of
  StateChange _ _ -> ([], seen)
  StateView _ _ _ -> ([], seen)
  InboundTranslation (_ :: Proxy handler) parts _ ->
    let handlerName = Types.typeName (Proxy @handler)
        expected = Types.inboundTargets (Proxy @(InboundOutputsOf handler))
        declared = [targetTypes target | InboundCommand target <- parts]
        missing =
          [ [fmt|#{path} omits inbound worker output #{commandName} -> #{entityName} of handler #{handlerName}|]
          | (commandType, entityType, commandName, entityName) <- expected,
            Prelude.notElem (commandType, entityType) declared
          ]
        repeated =
          [ [fmt|#{path} repeats inbound worker output #{commandName} -> #{entityName} of handler #{handlerName}|]
          | (commandType, entityType, commandName, entityName) <- expected,
            Prelude.length (Prelude.filter (== (commandType, entityType)) declared) > 1
          ]
    in  (missing Prelude.++ repeated, seen)
  OutboundTranslation (proxy :: Proxy handler) parts _ ->
    let triggerCount = countOutboundEvents parts
        triggerProblems = if triggerCount == 1 then [] else [[fmt|#{path} requires exactly one outbound onEvent; found #{triggerCount}|]]
        handlerType = Reflection.SomeTypeRep (Reflection.typeRep @handler)
        handlerName = Types.typeName proxy
        prior = [priorPath | (priorType, priorPath) <- seen, priorType == handlerType]
        reuseProblems =
          if Prelude.null prior
            then []
            else
              [ [fmt|#{path} reuses outbound handler #{handlerName}; first declared at #{Text.joinWith ", " (Array.fromLinkedList prior)}|]
              ]
    in  (triggerProblems Prelude.++ reuseProblems, (handlerType, path) : seen)


targetTypes :: CommandTarget -> (Reflection.SomeTypeRep, Reflection.SomeTypeRep)
targetTypes (CommandTarget (_ :: Proxy command) (_ :: Proxy entity)) =
  (Reflection.SomeTypeRep (Reflection.typeRep @command), Reflection.SomeTypeRep (Reflection.typeRep @entity))


countOutboundEvents :: [OutboundPart handler] -> Int
countOutboundEvents parts = case parts of
  [] -> 0
  OutboundEvent _ : rest -> 1 + countOutboundEvents rest
  OutboundCommand _ : rest -> countOutboundEvents rest
  OutboundLifecycle _ : rest -> countOutboundEvents rest


-- | Keep each case as one typed script with shared bindings.
rule :: Text -> RuleAction context () -> Writer [SlicePart context] ()
rule name script = Writer.tell [SliceRule Rule {ruleName = name, ruleScript = script}]


-- | Request a typed value. The rule runner supplies its concrete value.
requestValue :: forall value context. (Typeable value) => RuleAction context value
requestValue = RuleRequest (Proxy @value)


given :: Writer [HistoricalEvent] () -> RuleAction context ()
given builder = recordRuleParts (GivenStart : [GivenEvent value | value <- Writer.execWriter builder])


recordRuleParts :: [RulePart context] -> RuleAction context ()
recordRuleParts parts = case parts of
  [] -> RuleDone ()
  part : rest -> do
    RuleRecord part
    recordRuleParts rest


givenNothing :: RuleAction context ()
givenNothing = RuleRecord GivenNothing


class RuleInput context input where
  makeWhen :: input -> WhenValue context


instance RuleInput ('Change entity command) command where
  makeWhen value = CommandWhen value


instance
  ( Entity entity,
    EntityOf payload ~ entity,
    EventVariantOf (EventOf entity) payload,
    QueryOf entity query,
    QuerySourceMember entity query (EntitiesOf query),
    Typeable entity,
    Typeable payload
  ) =>
  RuleInput ('View query) payload
  where
  makeWhen value = EventWhen value


when :: forall context input. (RuleInput context input) => input -> RuleAction context ()
when value = RuleRecord (WhenPart (makeWhen value))


class RuleEventPart part payload where
  makeRuleEvent :: payload -> part


instance
  ( Entity (EntityOf payload),
    EventVariantOf (EventOf (EntityOf payload)) payload,
    Typeable (EntityOf payload),
    Typeable payload
  ) =>
  RuleEventPart HistoricalEvent payload
  where
  makeRuleEvent value = HistoricalEvent (Proxy @(EntityOf payload)) value


instance
  (mode ~ 'Types.Events, EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) =>
  RuleEventPart (OutcomePart ('Change entity command) mode) payload
  where
  makeRuleEvent value = OutcomeEvent (ExpectedEvent value)


-- | Add a concrete historical fixture or expected emitted payload.
event :: forall part payload. (RuleEventPart part payload) => payload -> Writer [part] ()
event value = Writer.tell [makeRuleEvent value]


class RuleOutcome context mode where
  makeExpected :: [OutcomePart context mode] -> Result Text (ExpectedOutcome context)


instance RuleOutcome ('Change entity command) 'Types.Events where
  makeExpected parts = case parts of
    [] -> Err "event outcome requires at least one event; use acceptedWithNoEvents for zero events"
    _ -> Ok (CommandExpected (Emitted [value | OutcomeEvent value <- parts]))


instance RuleOutcome ('Change entity command) 'Types.Rejection where
  makeExpected parts = case parts of
    [OutcomeRejected reason] -> Ok (CommandExpected (RejectedReason reason))
    [OutcomeRejectedPattern patternText] -> Ok (CommandExpected (RejectedPattern patternText))
    _ -> Err "rejection outcome requires exactly one rejected assertion"


instance RuleOutcome ('Change entity command) 'Types.ZeroEvents where
  makeExpected parts = case parts of
    [OutcomeAcceptedWithoutEvents] -> Ok (CommandExpected AcceptedWithoutEvents)
    _ -> Err "zero-event outcome requires exactly one acceptedWithNoEvents assertion"


instance RuleOutcome ('View query) 'Types.ViewUpdate where
  makeExpected parts = case parts of
    [OutcomeUpdated value] -> Ok (ViewExpected (Update value))
    _ -> Err "view update outcome requires exactly one updatedTo assertion"


instance RuleOutcome ('View query) 'Types.ViewDelete where
  makeExpected parts = case parts of
    [OutcomeDeleted] -> Ok (ViewExpected Delete)
    _ -> Err "view delete outcome requires exactly one deleted assertion"


instance RuleOutcome ('View query) 'Types.ViewNoOpMode where
  makeExpected parts = case parts of
    [OutcomeNoOp] -> Ok (ViewExpected NoOp)
    _ -> Err "view no-op outcome requires exactly one noOp assertion"


therefore ::
  forall context mode. (RuleOutcome context mode) => Writer [OutcomePart context mode] () -> RuleAction context ()
therefore builder = case makeExpected (Writer.execWriter builder) of
  Ok expected -> RuleRecord (ThereforePart expected)
  Err problem -> RuleRecord (InvalidOutcome problem)


rejected :: Writer [OutcomePart ('Change entity command) 'Types.Rejection] ()
rejected = Writer.tell [OutcomeRejected Nothing]


rejectedWith :: Text -> Writer [OutcomePart ('Change entity command) 'Types.Rejection] ()
rejectedWith reason = Writer.tell [OutcomeRejected (Just reason)]


rejectedMatches :: Text -> Writer [OutcomePart ('Change entity command) 'Types.Rejection] ()
rejectedMatches patternText = Writer.tell [OutcomeRejectedPattern patternText]


regex :: QuasiQuoter
regex =
  QuasiQuoter
    { quoteExp = \source -> [|Text.fromLinkedList source|],
      quotePat = \_ -> Prelude.fail "regex is an expression",
      quoteType = \_ -> Prelude.fail "regex is an expression",
      quoteDec = \_ -> Prelude.fail "regex is an expression"
    }


acceptedWithNoEvents :: Writer [OutcomePart ('Change entity command) 'Types.ZeroEvents] ()
acceptedWithNoEvents = Writer.tell [OutcomeAcceptedWithoutEvents]


updatedTo :: query -> Writer [OutcomePart ('View query) 'Types.ViewUpdate] ()
updatedTo value = Writer.tell [OutcomeUpdated value]


deleted :: forall query. Writer [OutcomePart ('View query) 'Types.ViewDelete] ()
deleted = Writer.tell [OutcomeDeleted]


noOp :: forall query. Writer [OutcomePart ('View query) 'Types.ViewNoOpMode] ()
noOp = Writer.tell [OutcomeNoOp]


ruleCase :: Text -> [RulePart context] -> Result Text (RuleCase context)
ruleCase name parts = do
  let setups = [() | GivenStart <- parts] Prelude.++ [() | GivenNothing <- parts]
  let historical = [value | GivenEvent value <- parts]
  let inputs = [value | WhenPart value <- parts]
  let outcomes = [value | ThereforePart value <- parts]
  let invalid = [problem | InvalidOutcome problem <- parts]
  case invalid of
    problem : _ -> Err [fmt|Rule "#{name}", #{problem}|]
    [] -> Ok ()
  if Prelude.length setups == 1 then Ok () else Err [fmt|Rule "#{name}" requires exactly one given or givenNothing|]
  input <- case inputs of
    [value] -> Ok value
    _ -> Err [fmt|Rule "#{name}" requires exactly one when|]
  expected <- case outcomes of
    [value] -> Ok value
    _ -> Err [fmt|Rule "#{name}" requires exactly one therefore|]
  Ok RuleCase {ruleHistory = historical, ruleWhen = input, ruleExpected = expected}
