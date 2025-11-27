module Service.Model.Core (
  Model (..),
  command,
  extract,
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


-- | Model represents an event-sourced application model definition
-- It tracks commands in a monadic DSL
-- The first type parameter is a type-level list tracking command types
data Model (commands :: Record.Row Type) a = Model
  { value :: a,
    commandNames :: Record commands
  }
  deriving (Generic)


-- | Apply a function to the value inside a Model
fmap :: forall cmds a b. (a -> b) -> Model cmds a -> Model cmds b
fmap f m =
  Model
    { value = f m.value,
      commandNames = m.commandNames
    }


-- | Create a Model with just a value
pureValue :: forall a. a -> Model '[] a
pureValue a =
  Model
    { value = a,
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
      commandNames = Record.merge m.commandNames result.commandNames
    }


-- | Combine two Models, keeping the second's value and merging their metadata
appendModel :: forall cmds1 cmds2 a. Model cmds1 a -> Model cmds2 a -> Model (Record.Merge cmds1 cmds2) a
appendModel m1 m2 =
  Model
    { value = m2.value,
      commandNames = Record.merge m1.commandNames m2.commandNames
    }


-- | Create an empty Model with a default value
emptyModel :: forall a. (Default a) => Model '[] a
emptyModel =
  Model
    { value = Default.def,
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
      commandNames =
        Record.insert field CommandDefinition Record.empty
    }


-- | Extract the value from a Model
extract :: forall cmds a. Model cmds a -> a
extract m = m.value
