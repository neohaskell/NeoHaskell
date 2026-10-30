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
  QuerySourceMember,
  primaryKind,
  typeName,
) where

import Basics
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import EventVariantOf (EventVariantOf)
import GHC.TypeLits qualified as GHC
import Result (Result)
import Service.Command.Core (Command)
import Service.Entity.Core (Entity, EntityOf, EventOf)
import Service.InboundIntegration.Core (InboundIntegration)
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Query.Core (EntitiesOf, Query, QueryOf)
import Text (Text)
import Text qualified
import Type.Reflection (Typeable)
import Type.Reflection qualified as Reflection


data EventModel = EventModel
  { modelName :: Text,
    modelFeatures :: [Feature]
  }


data Feature = Feature
  { featureName :: Text,
    featureChapters :: [Chapter]
  }


data Chapter = Chapter
  { chapterName :: Text,
    chapterSlices :: [Slice]
  }


data Slice = Slice
  { sliceName :: Text,
    slicePrimary :: Primary
  }


data Primary where
  StateChange :: (Entity entity, Typeable entity) => Proxy entity -> CommandRoot entity -> Primary
  StateView :: (Query query, Typeable query) => Proxy query -> [ReadSource query] -> Primary
  OutboundTranslation :: (OutboundIntegration handler, Typeable handler) => Proxy handler -> [OutboundPart handler] -> Primary
  InboundTranslation :: (InboundIntegration handler, Typeable handler) => Proxy handler -> [InboundPart handler] -> Primary


data CommandRoot entity where
  CommandRoot :: (Command command, EntityOf command ~ entity, Typeable command) => Proxy command -> [Produced entity] -> CommandRoot entity


data Produced entity where
  Produced :: (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => Proxy payload -> Produced entity


data ReadSource query where
  ReadSource :: (Entity entity, QueryOf entity query, QuerySourceMember entity query (EntitiesOf query), Typeable entity) => Proxy entity -> [EventRef entity] -> ReadSource query


data EventRef entity where
  EventRef :: (EntityOf payload ~ entity, EventVariantOf (EventOf entity) payload, Typeable payload) => Proxy payload -> EventRef entity


type family QuerySourceMember (entity :: Type) (query :: Type) (entities :: [Type]) :: Constraint where
  QuerySourceMember entity query '[] = GHC.TypeError (GHC.Text "Query source " GHC.:<>: GHC.ShowType entity GHC.:<>: GHC.Text " is absent from EntitiesOf " GHC.:<>: GHC.ShowType query)
  QuerySourceMember entity query (entity ': rest) = ()
  QuerySourceMember entity query (other ': rest) = QuerySourceMember entity query rest


data CommandTarget where
  CommandTarget :: (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) => Proxy command -> Proxy entity -> CommandTarget


data CommandTargetFor command where
  CommandTargetFor :: (Command command, Entity entity, EntityOf command ~ entity, Typeable command, Typeable entity) => Proxy entity -> CommandTargetFor command


data OutboundPart handler where
  OutboundEvent :: (EntityOf payload ~ EntityOf handler, HandledEvent handler ~ payload, EventVariantOf (EventOf (EntityOf handler)) payload, Typeable payload) => Proxy payload -> OutboundPart handler
  OutboundCommand :: CommandTarget -> OutboundPart handler


data InboundPart handler where
  InboundCommand :: CommandTarget -> InboundPart handler


data SlicePart = SlicePrimary Primary


data ModelPart
  = ModelName Text
  | ModelFeatures [Result Text Feature]


primaryKind :: Primary -> Text
primaryKind primary = case primary of
  StateChange _ _ -> "state-change"
  StateView _ _ -> "state-view"
  OutboundTranslation _ _ -> "outbound"
  InboundTranslation _ _ -> "inbound"


typeName :: forall value. (Typeable value) => Proxy value -> Text
typeName _ = Reflection.typeRep @value |> Reflection.typeRepTyCon |> Reflection.tyConName |> Text.fromLinkedList
