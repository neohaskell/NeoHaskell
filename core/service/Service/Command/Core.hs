{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  Command (..),
  CommandResult (..),
  EventOf,
  EntityOf,
) where

import Array (Array)
import Basics
import Maybe (Maybe)
import Service.Event (InsertionType)
import Text (Text)
import Uuid (Uuid)


data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


type family EntityOf (command :: Type) :: Type


class Command command where
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False


  type EntityIdType command :: Type
  type EntityIdType command = Text


  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType command)


  decideImpl :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))


type family GetEntityIdFunction (isTenant :: Bool) command id where
  GetEntityIdFunction False command id =
    command -> Maybe id
  GetEntityIdFunction True command id =
    Uuid -> command -> Maybe id


type family DecideFunction (isTenant :: Bool) command entity event where
  DecideFunction 'False command entity event =
    command -> Maybe entity -> CommandResult event
  DecideFunction 'True command entity event =
    Uuid -> command -> Maybe entity -> CommandResult event


type family EventOf (entityType :: Type)