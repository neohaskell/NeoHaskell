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
  sliceIsPlanned,
  sliceEventTypes,
  sliceCommandTargets,
  sliceEventSources,
  eventModel,
  appName,
  features,
  feature,
  chapter,
  slice,
  entity,
  plannedEntity,
  command,
  plannedCommand,
  produces,
  plannedProduces,
  query,
  plannedQuery,
  readsFrom,
  plannedReadsFrom,
  onEvent,
  plannedOnEvent,
  outboundIntegration,
  inboundIntegration,
  triggersCommand,
  onTargetEntity,
) where

import Basics
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer
import Data.Proxy (Proxy (..))
import EventVariantOf (EventVariantOf)
import Integration qualified
import Prelude qualified
import Result (Result (..))
import Service.Command.Core (Command)
import Service.Entity.Core (Entity, EntityOf, EventOf)
import Service.EventModel.Types (Chapter (..), CommandRoot (..), CommandTarget (..), CommandTargetFor (..), EventModel (..), EventRef (..), Feature (..), InboundPart (..), ModelPart (..), OutboundPart (..), PlannedCommandRoot (..), PlannedEventRef (..), PlannedProduced (..), PlannedReadSource (..), Primary (..), Produced (..), QuerySourceMember, ReadSource (..), Slice (..), SlicePart (..))
import Service.EventModel.Types qualified as Types
import Service.InboundIntegration.Core (InboundIntegration (..))
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Query.Core (EntitiesOf, Query, QueryOf)
import Text (Text)
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection


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
sliceName selected = selected.sliceName


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
slice :: Text -> Writer [SlicePart] () -> Writer [Result Text Slice] ()
slice name builder = do
  let primaries = [primary | SlicePrimary primary <- Writer.execWriter builder]
  let primaryCount = Prelude.length primaries
  let result = case primaries of
        [primary] -> Ok Slice {sliceName = name, slicePrimary = primary}
        _ -> Err [fmt|Slice "#{name}" requires exactly one primary; found #{primaryCount}|]
  Writer.tell [result]


entity :: forall entity. (Entity entity, Typeable entity) => CommandRoot entity -> Writer [SlicePart] ()
entity root = Writer.tell [SlicePrimary (StateChange (Proxy @entity) root)]


-- | Keep a state-change component in the authoring topology without requiring its implementation.
plannedEntity :: forall entity. Typeable entity => PlannedCommandRoot entity -> Writer [SlicePart] ()
plannedEntity root = Writer.tell [SlicePrimary (PlannedStateChange (Proxy @entity) root)]


command :: forall command entity. (Command command, EntityOf command ~ entity, Typeable command) => Writer [Produced entity] () -> CommandRoot entity
command builder = CommandRoot (Proxy @command) (Writer.execWriter builder)


-- | Reference a planned command whose target is declared with 'EntityOf'.
plannedCommand :: forall command entity. (EntityOf command ~ entity, Typeable command) => Writer [PlannedProduced entity] () -> PlannedCommandRoot entity
plannedCommand builder = PlannedCommandRoot (Proxy @command) (Writer.execWriter builder)


produces :: forall payload entity. (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => Writer [Produced entity] ()
produces = Writer.tell [Produced (Proxy @payload)]


-- | Reference a planned event payload before its Event ADT exists.
plannedProduces :: forall payload entity. (EntityOf payload ~ entity, Typeable payload) => Writer [PlannedProduced entity] ()
plannedProduces = Writer.tell [PlannedProduced (Proxy @payload)]


query :: forall query. (Query query, Typeable query) => Writer [ReadSource query] () -> Writer [SlicePart] ()
query builder = Writer.tell [SlicePrimary (StateView (Proxy @query) (Writer.execWriter builder))]


-- | Keep a state-view component in the authoring topology without requiring its implementation.
plannedQuery :: forall query. Typeable query => Writer [PlannedReadSource query] () -> Writer [SlicePart] ()
plannedQuery builder = Writer.tell [SlicePrimary (PlannedStateView (Proxy @query) (Writer.execWriter builder))]


readsFrom :: forall entity query. (Entity entity, QueryOf entity query, QuerySourceMember entity query (EntitiesOf query), Typeable entity) => Writer [EventRef entity] () -> Writer [ReadSource query] ()
readsFrom builder = Writer.tell [ReadSource (Proxy @entity) (Writer.execWriter builder)]


-- | Name a planned query source that belongs to the query's complete 'EntitiesOf' list.
plannedReadsFrom :: forall entity query. (QuerySourceMember entity query (EntitiesOf query), Typeable entity) => Writer [PlannedEventRef entity] () -> Writer [PlannedReadSource query] ()
plannedReadsFrom builder = Writer.tell [PlannedReadSource (Proxy @entity) (Writer.execWriter builder)]


class EventPart part payload where
  makeEventPart :: Proxy payload -> part


instance (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => EventPart (EventRef entity) payload where
  makeEventPart _ = EventRef (Proxy @payload)


instance (EntityOf payload ~ EntityOf handler, HandledEvent handler ~ payload, EventVariantOf (EventOf (EntityOf handler)) payload, Typeable payload) => EventPart (OutboundPart handler) payload where
  makeEventPart _ = OutboundEvent (Proxy @payload)


-- | Name a concrete payload subscription in a query source or outbound handler.
onEvent :: forall payload part. (EventPart part payload) => Writer [part] ()
onEvent = Writer.tell [makeEventPart @part @payload Proxy]


-- | Name a planned payload subscription without requiring its Event ADT mapping.
plannedOnEvent :: forall payload entity. (EntityOf payload ~ entity, Typeable payload) => Writer [PlannedEventRef entity] ()
plannedOnEvent = Writer.tell [PlannedEventRef (Proxy @payload)]


outboundIntegration :: forall handler. (OutboundIntegration handler, Typeable handler) => Writer [OutboundPart handler] () -> Writer [SlicePart] ()
outboundIntegration builder = Writer.tell [SlicePrimary (OutboundTranslation (Proxy @handler) (Writer.execWriter builder))]


inboundIntegration :: forall handler. (InboundIntegration handler, Typeable handler) => Writer [InboundPart handler] () -> Writer [SlicePart] ()
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
sliceKind selected = Types.primaryKind selected.slicePrimary


-- | Report whether this Slice is topology only and must not start a runtime component.
sliceIsPlanned :: Slice -> Bool
sliceIsPlanned selected = case selected.slicePrimary of
  PlannedStateChange _ _ -> True
  PlannedStateView _ _ -> True
  StateChange _ _ -> False
  StateView _ _ -> False
  OutboundTranslation _ _ -> False
  InboundTranslation _ _ -> False


sliceEventTypes :: Slice -> [Text]
sliceEventTypes selected = case selected.slicePrimary of
  StateChange _ (CommandRoot _ produced) -> [Types.typeName payload | Produced payload <- produced]
  StateView _ sources -> Prelude.concat [eventNames events | ReadSource _ events <- sources]
  PlannedStateChange _ (PlannedCommandRoot _ produced) -> [Types.typeName payload | PlannedProduced payload <- produced]
  PlannedStateView _ sources -> Prelude.concat [plannedEventNames events | PlannedReadSource _ events <- sources]
  OutboundTranslation _ parts -> [Types.typeName payload | OutboundEvent payload <- parts]
  InboundTranslation _ _ -> []


sliceCommandTargets :: Slice -> [(Text, Text)]
sliceCommandTargets selected = case selected.slicePrimary of
  StateChange entityProxy (CommandRoot commandProxy _) -> [(Types.typeName commandProxy, Types.typeName entityProxy)]
  StateView _ _ -> []
  PlannedStateChange entityProxy (PlannedCommandRoot commandProxy _) -> [(Types.typeName commandProxy, Types.typeName entityProxy)]
  PlannedStateView _ _ -> []
  OutboundTranslation _ parts -> [targetNames target | OutboundCommand target <- parts]
  InboundTranslation _ parts -> [targetNames target | InboundCommand target <- parts]


sliceEventSources :: Slice -> [(Text, [Text])]
sliceEventSources selected = case selected.slicePrimary of
  StateView _ sources -> [(Types.typeName entityProxy, eventNames events) | ReadSource entityProxy events <- sources]
  PlannedStateView _ sources -> [(Types.typeName entityProxy, plannedEventNames events) | PlannedReadSource entityProxy events <- sources]
  _ -> []


eventNames :: [EventRef entity] -> [Text]
eventNames events = [Types.typeName payload | EventRef payload <- events]


plannedEventNames :: [PlannedEventRef entity] -> [Text]
plannedEventNames events = [Types.typeName payload | PlannedEventRef payload <- events]


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
validateSlice featureLabel chapterLabel selected seen = case selected.slicePrimary of
  StateChange _ _ -> Ok seen
  StateView _ _ -> Ok seen
  PlannedStateChange _ _ -> Ok seen
  PlannedStateView _ _ -> Ok seen
  InboundTranslation _ _ -> Ok seen
  OutboundTranslation (proxy :: Proxy handler) parts -> do
    let triggerCount = countOutboundEvents parts
    let sliceLabel = selected.sliceName
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
