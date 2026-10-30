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
  appName,
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

import Basics
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer
import Data.Dynamic (Dynamic)
import Data.Dynamic qualified as Dynamic
import Data.Proxy (Proxy (..))
import EventVariantOf (EventVariantOf)
import Integration qualified
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Maybe (Maybe (..))
import Map qualified
import Prelude qualified
import Result (Result (..))
import Service.Command.Core (Command)
import Service.Entity.Core (Entity, EntityOf, EventOf)
import Service.EventModel.Types (Chapter (..), CommandExpectation (..), CommandRoot (..), CommandTarget (..), CommandTargetFor (..), EventModel (..), EventRef (..), ExpectedEvent (..), ExpectedOutcome (..), Feature (..), HistoricalEvent (..), InboundPart (..), ModelPart (..), OutcomePart (..), OutboundPart (..), Primary (..), Produced (..), QueryDiff (..), QuerySnapshot (..), QuerySourceMember, ReadSource (..), Rule (..), RuleAction (..), RuleCase (..), RulePart (..), RuleSample (..), Slice (..), SliceContext (..), SlicePart (..), ViewObservation (..), WhenValue (..))
import Service.EventModel.Types qualified as Types
import Service.InboundIntegration.Core (InboundIntegration (..))
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Query.Core (EntitiesOf, Query, QueryOf)
import Service.Query.Core (QueryAction (..))
import Text (Text)
import Text qualified
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection
import Uuid (Uuid)


-- | Export one of these actions from a component's Feature module.
type FeatureAction = Writer [Result Text Feature] ()


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
  StateChange entityProxy (CommandRoot commandProxy _) ->
    [do
      selectedCase <- sampleRule values selected
      Ok (ChangeSample entityProxy commandProxy selected.ruleName selectedCase)
    | (values, selected) <- suppliedRules]
  StateView queryProxy sources ->
    [do
      selectedCase <- sampleRule values selected
      validateViewSubscription selected.ruleName sources selectedCase
      Ok (ViewSample queryProxy selected.ruleName selectedCase)
    | (values, selected) <- suppliedRules]
  OutboundTranslation _ _ -> []
  InboundTranslation _ _ -> []


sampleRule :: [Dynamic] -> Rule context -> Result Text (RuleCase context)
sampleRule values selected = do
  (_, unused, parts) <- runRuleAction values selected.ruleScript
  let name = selected.ruleName
  if Prelude.null unused
    then Ok ()
    else Err [fmt|Rule "#{name}" received unused values|]
  ruleCase name parts


runRuleAction :: forall context value. [Dynamic] -> RuleAction context value -> Result Text (value, [Dynamic], [RulePart context])
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
    let declared = [Reflection.SomeTypeRep (Reflection.typeRep @eventPayload) | ReadSource _ events <- sources, EventRef (_ :: Proxy eventPayload) <- events]
    let incoming = Reflection.SomeTypeRep (Reflection.typeRep @payload)
    if Prelude.elem incoming declared
      then Ok ()
      else Err [fmt|Rule "#{name}" when payload #{Types.typeName (Proxy @payload)} is not listed by the query's onEvent subscriptions|]


-- | Assemble a named topology and reject malformed local structure.
-- Evaluation does not derive Event ADTs or start application workers.
eventModel :: EventModelAction -> Result Text EventModel
eventModel builder = do
  let parts = Writer.execWriter builder
  let names = [name | ModelName name <- parts]
  allFeatures <- Prelude.concat [found | ModelFeatures found <- parts] |> Prelude.sequence
  case names of
    [name] -> validateModel EventModel {modelName = name, modelFeatures = allFeatures}
    _ -> Err "eventModel requires exactly one appName"


appName :: Text -> EventModelAction
appName name = Writer.tell [ModelName name]


features :: FeatureAction -> EventModelAction
features builder = Writer.tell [ModelFeatures (Writer.execWriter builder)]


feature :: Text -> Writer [Result Text Chapter] () -> FeatureAction
feature name builder = do
  let found = Writer.execWriter builder |> Prelude.sequence
  let result = case found of
        Ok chapters -> Ok Feature {featureName = name, featureChapters = chapters}
        Err problem -> Err [fmt|Feature "#{name}", #{problem}|]
  Writer.tell [result]


chapter :: Text -> Writer [Result Text Slice] () -> Writer [Result Text Chapter] ()
chapter name builder = do
  let found = Writer.execWriter builder |> Prelude.sequence
  let result = case found of
        Ok slices -> Ok Chapter {chapterName = name, chapterSlices = slices}
        Err problem -> Err [fmt|Chapter "#{name}", #{problem}|]
  Writer.tell [result]


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
        _ -> Err [fmt|Slice "#{name}" requires exactly one primary; found #{primaryCount}|]
  Writer.tell [result]


entity :: forall entity command. (Entity entity, Typeable entity) => CommandRoot entity command -> Writer [SlicePart ('Change entity command)] ()
entity root = Writer.tell [SlicePrimary (StateChange (Proxy @entity) root)]


command :: forall command entity. (Command command, EntityOf command ~ entity, Typeable command) => Writer [Produced entity] () -> CommandRoot entity command
command builder = CommandRoot (Proxy @command) (Writer.execWriter builder)


produces :: forall payload entity. (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => Writer [Produced entity] ()
produces = Writer.tell [Produced (Proxy @payload)]


query :: forall query. (Query query, Typeable query) => Writer [ReadSource query] () -> Writer [SlicePart ('View query)] ()
query builder = Writer.tell [SlicePrimary (StateView (Proxy @query) (Writer.execWriter builder))]


readsFrom :: forall entity query. (Entity entity, QueryOf entity query, QuerySourceMember entity query (EntitiesOf query), Typeable entity) => Writer [EventRef entity] () -> Writer [ReadSource query] ()
readsFrom builder = Writer.tell [ReadSource (Proxy @entity) (Writer.execWriter builder)]


class EventPart part payload where
  makeEventPart :: Proxy payload -> part


instance (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => EventPart (EventRef entity) payload where
  makeEventPart _ = EventRef (Proxy @payload)


instance (EntityOf payload ~ EntityOf handler, HandledEvent handler ~ payload, EventVariantOf (EventOf (EntityOf handler)) payload, Typeable payload) => EventPart (OutboundPart handler) payload where
  makeEventPart _ = OutboundEvent (Proxy @payload)


-- | Name a concrete payload subscription in a query source or outbound handler.
onEvent :: forall payload part. (EventPart part payload) => Writer [part] ()
onEvent = Writer.tell [makeEventPart @part @payload Proxy]


outboundIntegration :: forall handler. (OutboundIntegration handler, Typeable handler) => Writer [OutboundPart handler] () -> Writer [SlicePart ('Outbound handler)] ()
outboundIntegration builder = Writer.tell [SlicePrimary (OutboundTranslation (Proxy @handler) (Writer.execWriter builder))]


inboundIntegration :: forall handler. (InboundIntegration handler, Typeable handler) => Writer [InboundPart handler] () -> Writer [SlicePart ('Inbound handler)] ()
inboundIntegration builder = Writer.tell [SlicePrimary (InboundTranslation (Proxy @handler) (Writer.execWriter builder))]


class CommandPart part command where
  makeCommandPart :: Proxy command -> CommandTarget -> part


instance CommandPart (OutboundPart handler) command where
  makeCommandPart _ target = OutboundCommand target


instance (InboundIntegration handler, Integration.AllowedOutput command (InboundOutputsOf handler)) => CommandPart (InboundPart handler) command where
  makeCommandPart _ target = InboundCommand target


triggersCommand :: forall command part. (CommandPart part command) => CommandTargetFor command -> Writer [part] ()
triggersCommand (CommandTargetFor (entityProxy :: Proxy entity)) = Writer.tell [makeCommandPart @part @command (Proxy @command) (CommandTarget (Proxy @command) entityProxy)]


onTargetEntity :: forall entity command. (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) => CommandTargetFor command
onTargetEntity = CommandTargetFor (Proxy @entity)


sliceKind :: Slice -> Text
sliceKind (Slice _ primary _) = Types.primaryKind primary


sliceEventTypes :: Slice -> [Text]
sliceEventTypes (Slice _ primary _) = case primary of
  StateChange _ (CommandRoot _ produced) -> [Types.typeName payload | Produced payload <- produced]
  StateView _ sources -> Prelude.concat [eventNames events | ReadSource _ events <- sources]
  OutboundTranslation _ parts -> [Types.typeName payload | OutboundEvent payload <- parts]
  InboundTranslation _ _ -> []


sliceCommandTargets :: Slice -> [(Text, Text)]
sliceCommandTargets (Slice _ primary _) = case primary of
  StateChange entityProxy (CommandRoot commandProxy _) -> [(Types.typeName commandProxy, Types.typeName entityProxy)]
  StateView _ _ -> []
  OutboundTranslation _ parts -> [targetNames target | OutboundCommand target <- parts]
  InboundTranslation _ parts -> [targetNames target | InboundCommand target <- parts]


sliceEventSources :: Slice -> [(Text, [Text])]
sliceEventSources (Slice _ primary _) = case primary of
  StateView _ sources -> [(Types.typeName entityProxy, eventNames events) | ReadSource entityProxy events <- sources]
  _ -> []


eventNames :: [EventRef entity] -> [Text]
eventNames events = [Types.typeName payload | EventRef payload <- events]


targetNames :: CommandTarget -> (Text, Text)
targetNames (CommandTarget commandProxy entityProxy) = (Types.typeName commandProxy, Types.typeName entityProxy)


validateModel :: EventModel -> Result Text EventModel
validateModel model = do
  if Prelude.null model.modelFeatures then Err "eventModel requires at least one Feature" else Ok ()
  validateFeatures model.modelFeatures []
  Ok model


validateFeatures :: [Feature] -> [Reflection.SomeTypeRep] -> Result Text ()
validateFeatures remaining seen = case remaining of
  [] -> Ok ()
  selected : rest -> do
    let featureLabel = selected.featureName
    if Prelude.null selected.featureChapters then Err [fmt|Feature "#{featureLabel}" requires a Chapter|] else Ok ()
    next <- validateChapters featureLabel selected.featureChapters seen
    validateFeatures rest next


validateChapters :: Text -> [Chapter] -> [Reflection.SomeTypeRep] -> Result Text [Reflection.SomeTypeRep]
validateChapters featureLabel remaining seen = case remaining of
  [] -> Ok seen
  selected : rest -> do
    let chapterLabel = selected.chapterName
    if Prelude.null selected.chapterSlices then Err [fmt|Feature "#{featureLabel}", Chapter "#{chapterLabel}" requires a Slice|] else Ok ()
    next <- validateSlices featureLabel chapterLabel selected.chapterSlices seen
    validateChapters featureLabel rest next


validateSlices :: Text -> Text -> [Slice] -> [Reflection.SomeTypeRep] -> Result Text [Reflection.SomeTypeRep]
validateSlices featureLabel chapterLabel remaining seen = case remaining of
  [] -> Ok seen
  selected : rest -> do
    next <- validateSlice featureLabel chapterLabel selected seen
    validateSlices featureLabel chapterLabel rest next


validateSlice :: Text -> Text -> Slice -> [Reflection.SomeTypeRep] -> Result Text [Reflection.SomeTypeRep]
validateSlice featureLabel chapterLabel selected@(Slice _ primary _) seen = case primary of
  StateChange _ _ -> Ok seen
  StateView _ _ -> Ok seen
  InboundTranslation _ _ -> Ok seen
  OutboundTranslation (proxy :: Proxy handler) parts -> do
    let triggerCount = countOutboundEvents parts
    let sliceLabel = sliceName selected
    if triggerCount == 1 then Ok () else Err [fmt|Feature "#{featureLabel}", Chapter "#{chapterLabel}", Slice "#{sliceLabel}" requires exactly one outbound onEvent; found #{triggerCount}|]
    let handlerType = Reflection.SomeTypeRep (Reflection.typeRep @handler)
    let handlerName = Types.typeName proxy
    if Prelude.elem handlerType seen
      then Err [fmt|Feature "#{featureLabel}", Chapter "#{chapterLabel}", Slice "#{sliceLabel}" reuses outbound handler #{handlerName}|]
      else Ok (handlerType : seen)


countOutboundEvents :: [OutboundPart handler] -> Int
countOutboundEvents parts = case parts of
  [] -> 0
  OutboundEvent _ : rest -> 1 + countOutboundEvents rest
  OutboundCommand _ : rest -> countOutboundEvents rest


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


instance (Entity entity, EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, QueryOf entity query, QuerySourceMember entity query (EntitiesOf query), Typeable entity, Typeable payload) => RuleInput ('View query) payload where
  makeWhen value = EventWhen value


when :: forall context input. (RuleInput context input) => input -> RuleAction context ()
when value = RuleRecord (WhenPart (makeWhen value))


class RuleEventPart part payload where
  makeRuleEvent :: payload -> part


instance (Entity (EntityOf payload), EventVariantOf (EventOf (EntityOf payload)) payload, Typeable (EntityOf payload), Typeable payload) => RuleEventPart HistoricalEvent payload where
  makeRuleEvent value = HistoricalEvent (Proxy @(EntityOf payload)) value


instance (mode ~ 'Types.Events, EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => RuleEventPart (OutcomePart ('Change entity command) mode) payload where
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


therefore :: forall context mode. (RuleOutcome context mode) => Writer [OutcomePart context mode] () -> RuleAction context ()
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
regex = QuasiQuoter
  { quoteExp = \source -> [| Text.fromLinkedList source |],
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
