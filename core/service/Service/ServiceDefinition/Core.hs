module Service.ServiceDefinition.Core (
  Service,
  ServiceDefinition (..),
  command,
  extract,
  yield,
  (>>=),
  pure,
  (<*>),
  appendServiceDefinition,
  emptyServiceDefinition,
  fmap,
  (>>),
  return,
  join,
) where

import Basics hiding (fmap, join, pure, return, (<*>), (>>), (>>=))
import Default (Default)
import Default qualified
import Function (unchanged)
import Record (Record)
import Record qualified
import Service.Command (NameOf)
import Service.Command.Core (Command)


type Service commands = ServiceDefinition commands Unit


-- | ServiceDefinition represents an event-sourced application service definition
-- It tracks commands in a monadic DSL
-- The first type parameter is a type-level list tracking command types
data
  ServiceDefinition
    (commands :: Record.Row Type)
    (value :: Type)
  = ServiceDefinition
  { commandNames :: Record commands,
    value :: value
  }
  deriving (Generic)


-- | Apply a function to the value inside a ServiceDefinition
fmap :: forall cmds a b. (a -> b) -> ServiceDefinition cmds a -> ServiceDefinition cmds b
fmap f m =
  ServiceDefinition
    { value = f m.value,
      commandNames = m.commandNames
    }


-- | Create a ServiceDefinition with just a value
pureValue :: forall a. a -> ServiceDefinition '[] a
pureValue a =
  ServiceDefinition
    { value = a,
      commandNames = Record.empty
    }


-- | Apply a function wrapped in a ServiceDefinition to a value wrapped in a ServiceDefinition
applyValue :: forall cmds a b. ServiceDefinition cmds (a -> b) -> ServiceDefinition cmds a -> ServiceDefinition cmds b
applyValue fn x = fn <*> x


-- | Sequentially compose two ServiceDefinitions, passing the value from the first as an argument to the second
bindValue ::
  forall cmds1 cmds2 a b.
  ServiceDefinition cmds1 a -> (a -> ServiceDefinition cmds2 b) -> ServiceDefinition (Record.Merge cmds1 cmds2) b
bindValue m f = do
  let result = f m.value
  ServiceDefinition
    { value = result.value,
      commandNames = Record.merge m.commandNames result.commandNames
    }


-- | Combine two ServiceDefinitions, keeping the second's value and merging their metadata
appendServiceDefinition ::
  forall cmds1 cmds2 a.
  ServiceDefinition cmds1 a -> ServiceDefinition cmds2 a -> ServiceDefinition (Record.Merge cmds1 cmds2) a
appendServiceDefinition m1 m2 =
  ServiceDefinition
    { value = m2.value,
      commandNames = Record.merge m1.commandNames m2.commandNames
    }


-- | Create an empty ServiceDefinition with a default value
emptyServiceDefinition :: forall a. (Default a) => ServiceDefinition '[] a
emptyServiceDefinition =
  ServiceDefinition
    { value = Default.def,
      commandNames = Record.empty
    }


pure :: a -> ServiceDefinition '[] a
pure = pureValue


return :: a -> ServiceDefinition '[] a
return = pure


(<*>) :: ServiceDefinition cmds (a -> b) -> ServiceDefinition cmds a -> ServiceDefinition cmds b
(<*>) = applyValue


(>>=) :: ServiceDefinition cmds1 a -> (a -> ServiceDefinition cmds2 b) -> ServiceDefinition (Record.Merge cmds1 cmds2) b
(>>=) = bindValue


(>>) :: ServiceDefinition cmds1 a -> (ServiceDefinition cmds2 b) -> ServiceDefinition (Record.Merge cmds1 cmds2) b
(>>) a b = bindValue a (\_ -> b)


-- | Flatten a nested ServiceDefinition structure
join ::
  forall cmds1 cmds2 a.
  ServiceDefinition cmds1 (ServiceDefinition cmds2 a) -> ServiceDefinition (Record.Merge cmds1 cmds2) a
join m = bindValue m unchanged


-- | Create a ServiceDefinition with just a value
yield :: forall a. a -> ServiceDefinition '[] a
yield a =
  ServiceDefinition
    { value = a,
      commandNames = Record.empty
    }


-- | Proxy to store the type of the command
data CommandDefinition (command :: Type) = CommandDefinition


-- | Register a command type in the service definition
command ::
  forall (commandType :: Type) (commandName :: Symbol).
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition '[commandName Record.:= CommandDefinition commandType] Unit
command = do
  let field = fromLabel @commandName
  let definition = CommandDefinition
  ServiceDefinition
    { value = unit,
      commandNames =
        Record.empty
          |> Record.insert field definition
    }


-- | Extract the value from a ServiceDefinition
extract :: forall cmds a. ServiceDefinition cmds a -> a
extract m = m.value
