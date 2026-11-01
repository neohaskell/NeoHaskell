-- | Represent the local Feature hierarchy and its typed component references.
-- The public builders in 'Service.EventModel' construct these values.
module Service.EventModel.Types (
  EventModel (..),
  Feature (..),
  Chapter (..),
  Slice (..),
  Primary (..),
  CommandRoot (..),
  Produced (..),
  ReadSource (..),
  EventRef (..),
  CommandTarget (..),
  CommandTargetFor (..),
  OutboundPart (..),
  InboundPart (..),
  SlicePart (..),
  ModelPart (..),
  SliceContext (..),
  Rule (..),
  RuleAction (..),
  RuleCase (..),
  RuleSample (..),
  RulePart (..),
  HistoricalEvent (..),
  WhenValue (..),
  ExpectedOutcome (..),
  CommandExpectation (..),
  ExpectedEvent (..),
  OutcomeMode (..),
  OutcomePart (..),
  QuerySnapshot (..),
  QueryDiff (..),
  ViewObservation (..),
  QuerySourceMember,
  KnownInboundTargets (..),
  primaryKind,
  typeName,
) where

import Basics
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import EventVariantOf (EventVariantOf)
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Maybe (Maybe)
import Service.Application (Application)
import Service.Command.Core (Command, NameOf)
import Service.Entity.Core (Entity (..), EntityOf, Event (..), EventOf)
import Service.Event.StreamId qualified as StreamId
import Service.InboundIntegration.Core (InboundIntegration (..))
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Query.Core (EntitiesOf, Query, QueryAction, QueryOf)
import Service.Query.Definition (WireEntities)
import Service.ServiceDefinition.Core (RunnableCommand, ServiceRunner)
import Text (Text)
import Text qualified
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection
import Uuid (Uuid)
import Prelude (Applicative (..), Functor (..), Monad (..))


data EventModel = EventModel
  { modelName :: Text,
    modelFeatures :: [Feature],
    modelConfiguration :: [Application -> Application]
  }


data Feature = Feature
  { featureName :: Text,
    featureChapters :: [Chapter]
  }


data Chapter = Chapter
  { chapterName :: Text,
    chapterSlices :: [Slice]
  }


data Slice where
  Slice :: Text -> Primary context -> [Rule context] -> Slice


data SliceContext = Change Type Type | View Type | Outbound Type | Inbound Type


data Primary context where
  StateChange ::
    (Entity entity, Typeable entity) => Proxy entity -> CommandRoot entity command -> Primary ('Change entity command)
  StateView ::
    (Query query, Typeable query, Json.ToJSON query, Json.FromJSON query, GHC.KnownSymbol (NameOf query), WireEntities (EntitiesOf query) query) =>
    Proxy query -> [ReadSource query] -> (Application -> Application) -> Primary ('View query)
  OutboundTranslation ::
    (OutboundIntegration handler, Typeable handler) =>
    Proxy handler -> [OutboundPart handler] -> (Application -> Application) -> Primary ('Outbound handler)
  InboundTranslation ::
    (InboundIntegration handler, KnownInboundTargets (InboundOutputsOf handler), Typeable handler) =>
    Proxy handler -> [InboundPart handler] -> (Application -> Application) -> Primary ('Inbound handler)


data CommandRoot entity command where
  CommandRoot ::
    (RunnableCommand command, EntityOf command ~ entity, Json.ToJSON (EventOf entity), Typeable command) =>
    Proxy command -> [Produced entity] -> ServiceRunner -> CommandRoot entity command


data Produced entity where
  Produced ::
    (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) =>
    Proxy payload -> Produced entity


data ReadSource query where
  ReadSource ::
    ( Entity entity,
      QueryOf entity query,
      QuerySourceMember entity query (EntitiesOf query),
      Typeable entity,
      GHC.KnownSymbol (NameOf entity),
      Json.FromJSON (EventOf entity)
    ) =>
    Proxy entity -> [EventRef entity] -> ReadSource query


data EventRef entity where
  EventRef ::
    (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) =>
    Proxy payload -> EventRef entity


type family QuerySourceMember (entity :: Type) (query :: Type) (entities :: [Type]) :: Constraint where
  QuerySourceMember entity query '[] =
    GHC.TypeError
      ( GHC.Text "Query source "
          GHC.:<>: GHC.ShowType entity
          GHC.:<>: GHC.Text " is absent from EntitiesOf "
          GHC.:<>: GHC.ShowType query
      )
  QuerySourceMember entity query (entity ': rest) = ()
  QuerySourceMember entity query (other ': rest) = QuerySourceMember entity query rest


data CommandTarget where
  CommandTarget ::
    (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) =>
    Proxy command -> Proxy entity -> CommandTarget


data CommandTargetFor command where
  CommandTargetFor ::
    (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) =>
    Proxy entity -> CommandTargetFor command


data OutboundPart handler where
  OutboundEvent ::
    ( EntityOf payload ~ EntityOf handler,
      HandledEvent handler ~ payload,
      EventVariantOf (EventOf (EntityOf handler)) payload,
      Typeable payload
    ) =>
    Proxy payload -> OutboundPart handler
  OutboundCommand :: CommandTarget -> OutboundPart handler
  OutboundLifecycle :: (Application -> Application) -> OutboundPart handler


data InboundPart handler where
  InboundCommand :: CommandTarget -> InboundPart handler


class KnownInboundTargets (outputs :: [(Type, Type)]) where
  inboundTargets :: Proxy outputs -> [(Reflection.SomeTypeRep, Reflection.SomeTypeRep, Text, Text)]


instance KnownInboundTargets '[] where
  inboundTargets _ = []


instance
  ( Command command,
    Entity entity,
    EntityOf command ~ entity,
    Typeable command,
    Typeable entity,
    KnownInboundTargets rest
  ) =>
  KnownInboundTargets ('(command, entity) ': rest)
  where
  inboundTargets _ =
    ( Reflection.SomeTypeRep (Reflection.typeRep @command),
      Reflection.SomeTypeRep (Reflection.typeRep @entity),
      typeName (Proxy @command),
      typeName (Proxy @entity)
    )
      : inboundTargets (Proxy @rest)


data SlicePart context
  = SlicePrimary (Primary context)
  | SliceRule (Rule context)


data Rule context = Rule
  { ruleName :: Text,
    ruleScript :: RuleAction context ()
  }


data RuleAction context value where
  RuleDone :: value -> RuleAction context value
  RuleBind :: RuleAction context input -> (input -> RuleAction context value) -> RuleAction context value
  RuleRequest :: (Typeable value) => Proxy value -> RuleAction context value
  RuleRecord :: RulePart context -> RuleAction context ()


instance Functor (RuleAction context) where
  fmap transform action = RuleBind action (\value -> RuleDone (transform value))


instance Applicative (RuleAction context) where
  pure = RuleDone
  function <*> input = RuleBind function (\transform -> RuleBind input (\value -> RuleDone (transform value)))


instance Monad (RuleAction context) where
  action >>= next = RuleBind action next


data RuleCase context = RuleCase
  { ruleHistory :: [HistoricalEvent],
    ruleWhen :: WhenValue context,
    ruleExpected :: ExpectedOutcome context
  }


data RuleSample where
  ChangeSample ::
    (Entity entity, Typeable entity, RunnableCommand command, Json.ToJSON (EventOf entity), Typeable command, EntityOf command ~ entity) =>
    Proxy entity -> Proxy command -> Text -> RuleCase ('Change entity command) -> RuleSample
  ViewSample ::
    (Query query, Typeable query, Json.ToJSON query, Json.FromJSON query, GHC.KnownSymbol (NameOf query), WireEntities (EntitiesOf query) query) =>
    Proxy query -> [ReadSource query] -> Text -> RuleCase ('View query) -> RuleSample


data HistoricalEvent where
  HistoricalEvent ::
    ( Entity entity,
      Event (EventOf entity),
      EntityOf (EventOf entity) ~ entity,
      EntityOf payload ~ entity,
      EventVariantOf (EventOf entity) payload,
      StreamId.ToStreamId (EntityIdType entity),
      GHC.KnownSymbol (NameOf entity),
      Json.ToJSON (EventOf entity),
      Typeable entity,
      Typeable payload
    ) =>
    Proxy entity -> payload -> HistoricalEvent


data WhenValue context where
  CommandWhen :: command -> WhenValue ('Change entity command)
  EventWhen ::
    ( Entity entity,
      Event (EventOf entity),
      EntityOf (EventOf entity) ~ entity,
      EntityOf payload ~ entity,
      EventVariantOf (EventOf entity) payload,
      StreamId.ToStreamId (EntityIdType entity),
      GHC.KnownSymbol (NameOf entity),
      Json.ToJSON (EventOf entity),
      QueryOf entity query,
      QuerySourceMember entity query (EntitiesOf query),
      Typeable entity,
      Typeable payload
    ) =>
    payload -> WhenValue ('View query)


data ExpectedEvent entity where
  ExpectedEvent ::
    (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) =>
    payload -> ExpectedEvent entity


data CommandExpectation entity
  = Emitted [ExpectedEvent entity]
  | RejectedReason (Maybe Text)
  | RejectedPattern Text
  | AcceptedWithoutEvents


data ExpectedOutcome context where
  CommandExpected :: CommandExpectation entity -> ExpectedOutcome ('Change entity command)
  ViewExpected :: QueryAction query -> ExpectedOutcome ('View query)


newtype QuerySnapshot query = QuerySnapshot (Map Uuid query)


data QueryDiff query = QueryDiff
  { before :: QuerySnapshot query,
    after :: QuerySnapshot query
  }


data ViewObservation query = ViewObservation
  { action :: QueryAction query,
    diff :: QueryDiff query
  }


data RulePart context
  = GivenStart
  | GivenEvent HistoricalEvent
  | GivenNothing
  | WhenPart (WhenValue context)
  | ThereforePart (ExpectedOutcome context)
  | InvalidOutcome Text


data OutcomeMode = Events | Rejection | ZeroEvents | ViewUpdate | ViewDelete | ViewNoOpMode


data OutcomePart context mode where
  OutcomeEvent :: ExpectedEvent entity -> OutcomePart ('Change entity command) 'Events
  OutcomeRejected :: Maybe Text -> OutcomePart ('Change entity command) 'Rejection
  OutcomeRejectedPattern :: Text -> OutcomePart ('Change entity command) 'Rejection
  OutcomeAcceptedWithoutEvents :: OutcomePart ('Change entity command) 'ZeroEvents
  OutcomeUpdated :: query -> OutcomePart ('View query) 'ViewUpdate
  OutcomeDeleted :: OutcomePart ('View query) 'ViewDelete
  OutcomeNoOp :: OutcomePart ('View query) 'ViewNoOpMode


data ModelPart
  = ModelName Text
  | ModelFeatures [([Text], Feature)]
  | ModelConfigRegistration (Application -> Application)
  | ModelConfiguration (Application -> Application)


primaryKind :: Primary context -> Text
primaryKind primary = case primary of
  StateChange _ _ -> "state-change"
  StateView _ _ _ -> "state-view"
  OutboundTranslation _ _ _ -> "outbound"
  InboundTranslation _ _ _ -> "inbound"


typeName :: forall value. (Typeable value) => Proxy value -> Text
typeName _ = Reflection.typeRep @value |> Reflection.typeRepTyCon |> Reflection.tyConName |> Text.fromLinkedList
