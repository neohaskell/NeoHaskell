{-# LANGUAGE AllowAmbiguousTypes #-}

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


-- | Model represents an event-sourced application model definition
-- It tracks entities, events, and commands in a monadic DSL
data Model a = Model
  { value :: a,
    entities :: Set Text,
    eventTypes :: Set Text,
    commands :: Set Text
  }
  deriving (Eq, Show, Generic)


instance Functor Model where
  fmap f m =
    m
      |> Thenable.andThen
        ( \r ->
            f r |> yield
        )


instance Applicative Model where
  pure = yield
  (<*>) = Monad.ap


instance Monad Model where
  m >>= f = do
    let result = f m.value
    Model
      { value = result.value,
        entities = m.entities |> Set.union result.entities,
        eventTypes = m.eventTypes |> Set.union result.eventTypes,
        commands = m.commands |> Set.union result.commands
      }


instance Semigroup (Model a) where
  m1 <> m2 =
    Model
      { value = m2.value,
        entities = m1.entities |> Set.union m2.entities,
        eventTypes = m1.eventTypes |> Set.union m2.eventTypes,
        commands = m1.commands |> Set.union m2.commands
      }


instance (Default a) => Monoid (Model a) where
  mempty =
    Model
      { value = Default.def,
        entities = Set.empty,
        eventTypes = Set.empty,
        commands = Set.empty
      }


-- | Create a Model with just a value
yield :: forall a. a -> Model a
yield a =
  Model
    { value = a,
      entities = Set.empty,
      eventTypes = Set.empty,
      commands = Set.empty
    }


-- | Register an entity type in the model
entity :: forall (entityType :: Type). (Typeable entityType) => Model Unit
entity = do
  let typeName = TypeName.reflect @entityType
  Model
    { value = unit,
      entities = Set.singleton typeName,
      eventTypes = Set.empty,
      commands = Set.empty
    }


-- | Register event types in the model
events :: forall (eventType :: Type). (Typeable eventType) => Model Unit
events = do
  let typeName = TypeName.reflect @eventType
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.singleton typeName,
      commands = Set.empty
    }


-- | Register a command type in the model
command :: forall (commandType :: Type). (Typeable commandType) => Model Unit
command = do
  let typeName = TypeName.reflect @commandType
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.empty,
      commands = Set.singleton typeName
    }


-- | Extract the value from a Model
extract :: Model a -> a
extract m = m.value


-- | Check if an entity type is registered in the model
hasEntity :: forall (entityType :: Type) a. (Typeable entityType) => Model a -> Bool
hasEntity m = do
  let typeName = TypeName.reflect @entityType
  Set.member typeName m.entities


-- | Check if an event type is registered in the model
hasEvents :: forall (eventType :: Type) a. (Typeable eventType) => Model a -> Bool
hasEvents m = do
  let typeName = TypeName.reflect @eventType
  Set.member typeName m.eventTypes


-- | Check if a command type is registered in the model
hasCommand :: forall (commandType :: Type) a. (Typeable commandType) => Model a -> Bool
hasCommand m = do
  let typeName = TypeName.reflect @commandType
  Set.member typeName m.commands


-- | Check if a model is empty (has no registered types)
isEmpty :: Model a -> Bool
isEmpty m =
  Set.isEmpty m.entities
    && Set.isEmpty m.eventTypes
    && Set.isEmpty m.commands
