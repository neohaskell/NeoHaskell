{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Model.Core (
  Model (..),
  entity,
  events,
  command,
  extract,
  hasEntity,
  hasEvents,
  yield,
  (>>=),
  pure,
  (<*>),
  appendModel,
  emptyModel,
  fmap,
  (>>),
  return,
  join,
) where

import Basics hiding (fmap, join, pure, return, (<*>), (>>), (>>=))
import Default (Default)
import Default qualified
import Record (Record)
import Record qualified
import Set (Set)
import Set qualified
import Text (Text)
import Type.Reflection (Typeable)
import TypeName qualified


-- | Model represents an event-sourced application model definition
-- It tracks entities, events, and commands in a monadic DSL
-- The first type parameter is a type-level list tracking command types
data Model (commands :: Record.Row Type) a = Model
  { value :: a,
    entities :: Set Text,
    eventTypes :: Set Text,
    commandNames :: Record commands
  }
  deriving (Generic)


-- | Apply a function to the value inside a Model
fmap :: forall cmds a b. (a -> b) -> Model cmds a -> Model cmds b
fmap f m =
  Model
    { value = f m.value,
      entities = m.entities,
      eventTypes = m.eventTypes,
      commandNames = m.commandNames
    }


-- | Create a Model with just a value
pureValue :: forall a. a -> Model '[] a
pureValue a =
  Model
    { value = a,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames = Record.empty
    }


-- | Apply a function wrapped in a Model to a value wrapped in a Model
applyValue :: forall cmds a b. Model cmds (a -> b) -> Model cmds a -> Model cmds b
applyValue fn x = fn <*> x


-- | Sequentially compose two Models, passing the value from the first as an argument to the second
bindValue :: forall cmds1 cmds2 a b. Model cmds1 a -> (a -> Model cmds2 b) -> Model (Record.Merge cmds1 cmds2) b
bindValue m f = do
  let result = f m.value
  Model
    { value = result.value,
      entities = m.entities |> Set.union result.entities,
      eventTypes = m.eventTypes |> Set.union result.eventTypes,
      commandNames = Record.merge m.commandNames result.commandNames
    }


-- | Combine two Models, keeping the second's value and merging their metadata
appendModel :: forall cmds1 cmds2 a. Model cmds1 a -> Model cmds2 a -> Model (Record.Merge cmds1 cmds2) a
appendModel m1 m2 =
  Model
    { value = m2.value,
      entities = m1.entities |> Set.union m2.entities,
      eventTypes = m1.eventTypes |> Set.union m2.eventTypes,
      commandNames = Record.merge m1.commandNames m2.commandNames
    }


-- | Create an empty Model with a default value
emptyModel :: forall a. (Default a) => Model '[] a
emptyModel =
  Model
    { value = Default.def,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames = Record.empty
    }


pure :: a -> Model '[] a
pure = pureValue


return :: a -> Model '[] a
return = pure


(<*>) :: Model cmds (a -> b) -> Model cmds a -> Model cmds b
(<*>) = applyValue


(>>=) :: Model cmds1 a -> (a -> Model cmds2 b) -> Model (Record.Merge cmds1 cmds2) b
(>>=) = bindValue


(>>) :: Model cmds1 a -> (Model cmds2 b) -> Model (Record.Merge cmds1 cmds2) b
(>>) a b = bindValue a (\_ -> b)


-- | Flatten a nested Model structure
join :: forall cmds1 cmds2 a. Model cmds1 (Model cmds2 a) -> Model (Record.Merge cmds1 cmds2) a
join m = bindValue m (\innerModel -> innerModel)


-- | Create a Model with just a value
yield :: forall a. a -> Model '[] a
yield a =
  Model
    { value = a,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames = Record.empty
    }


-- | Register an entity type in the model
entity :: forall (entityType :: Type). (Typeable entityType) => Model '[] Unit
entity = do
  let typeName = TypeName.reflect @entityType
  Model
    { value = unit,
      entities = Set.singleton typeName,
      eventTypes = Set.empty,
      commandNames = Record.empty
    }


-- | Register event types in the model
events :: forall (eventType :: Type). (Typeable eventType) => Model '[] Unit
events = do
  let typeName = TypeName.reflect @eventType
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.singleton typeName,
      commandNames = Record.empty
    }


-- | Proxy to store the type of the command
data CommandDefinition (command :: Type) = CommandDefinition


-- | Register a command type in the model
command ::
  forall (commandType :: Type) (commandName :: Symbol).
  Record.Field commandName -> Model '[commandName Record.:= CommandDefinition commandType] Unit
command field = do
  Model
    { value = unit,
      entities = Set.empty,
      eventTypes = Set.empty,
      commandNames =
        Record.insert field CommandDefinition Record.empty
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
