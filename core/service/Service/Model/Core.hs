{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.Model.Core (
  Model (..),
  entity,
  events,
  command,
  extract,
  hasEntity,
  hasEvents,
  hasCommand,
  isEmpty,
  yield,
) where

import Applicable (Applicative (..))
import Basics
import Control.Monad qualified as Monad
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Default (Default)
import Default qualified
import Mappable
import Set (Set)
import Set qualified
import Text (Text)
import Thenable hiding (yield)
import Type.Reflection (Typeable)
import TypeName qualified


-- | Type family for concatenating type-level lists
type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys


-- | Model represents an event-sourced application model definition
-- It tracks entities, events, and commands in a monadic DSL
-- The first type parameter is a type-level list tracking command types
data Model (commands :: [Type]) a = Model
  { value :: a,
    entities :: Set Text,
    eventTypes :: Set Text,
    commandNames :: Set Text
  }
  deriving (Eq, Show, Generic)


instance Functor (Model cmds) where
  fmap f m =
    Model
      { value = f m.value,
        entities = m.entities,
        eventTypes = m.eventTypes,
        commandNames = m.commandNames
      }


instance Applicative (Model cmds) where
  pure a =
    Model
      { value = a,
        entities = Set.empty,
        eventTypes = Set.empty,
        commandNames = Set.empty
      }
  (<*>) = Monad.ap


instance Monad (Model cmds) where
  m >>= f = do
    let result = f m.value
    Model
      { value = result.value,
        entities = m.entities |> Set.union result.entities,
        eventTypes = m.eventTypes |> Set.union result.eventTypes,
        commandNames = m.commandNames |> Set.union result.commandNames
      }


instance Semigroup (Model cmds a) where
  m1 <> m2 =
    Model
      { value = m2.value,
        entities = m1.entities |> Set.union m2.entities,
        eventTypes = m1.eventTypes |> Set.union m2.eventTypes,
        commandNames = m1.commandNames |> Set.union m2.commandNames
      }


instance (Default a) => Monoid (Model '[] a) where
  mempty =
    Model
      { value = Default.def,
        entities = Set.empty,
        eventTypes = Set.empty,
        commandNames = Set.empty
      }


-- | Create a Model with just a value
yield :: forall cmds a. a -> Model cmds a
yield a =
  Model
    { value = a,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames = Set.empty
    }


-- | Register an entity type in the model
entity :: forall (entityType :: Type) cmds. (Typeable entityType) => Model cmds Unit
entity = do
  let typeName = TypeName.reflect @entityType
  Model
    { value = unit,
      entities = Set.singleton typeName,
      eventTypes = Set.empty,
      commandNames = Set.empty
    }


-- | Register event types in the model
events :: forall (eventType :: Type) cmds. (Typeable eventType) => Model cmds Unit
events = do
  let typeName = TypeName.reflect @eventType
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.singleton typeName,
      commandNames = Set.empty
    }


-- | Register a command type in the model
command :: forall (commandType :: Type) cmds. (Typeable commandType) => Model cmds Unit
command = do
  let typeName = TypeName.reflect @commandType
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames = Set.singleton typeName
    }


-- | Extract the value from a Model
extract :: forall cmds a. Model cmds a -> a
extract m = m.value


-- | Check if an entity type is registered in the model
hasEntity :: forall (entityType :: Type) cmds a. (Typeable entityType) => Model cmds a -> Bool
hasEntity m = do
  let typeName = TypeName.reflect @entityType
  Set.member typeName m.entities


-- | Check if an event type is registered in the model
hasEvents :: forall (eventType :: Type) cmds a. (Typeable eventType) => Model cmds a -> Bool
hasEvents m = do
  let typeName = TypeName.reflect @eventType
  Set.member typeName m.eventTypes


-- | Check if a command type is registered in the model
hasCommand :: forall (commandType :: Type) cmds a. (Typeable commandType) => Model cmds a -> Bool
hasCommand m = do
  let typeName = TypeName.reflect @commandType
  Set.member typeName m.commandNames


-- | Check if a model is empty (has no registered types)
isEmpty :: forall cmds a. Model cmds a -> Bool
isEmpty m =
  Set.isEmpty m.entities
    && Set.isEmpty m.eventTypes
    && Set.isEmpty m.commandNames
