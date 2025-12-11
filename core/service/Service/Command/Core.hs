{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Command.Core (
  Command (..),
  CommandResult (..),
  EventOf,
  EntityOf,
  Decision (..),
  DecisionContext (..),
  ApiEndpointHandler,
  runDecision,
  NameOf,
  ApiOf,
  ApiBuilder,
  WebApi (..),
  buildCommandHandler,
) where

import Applicable
import Array (Array)
import Basics
import Bytes (Bytes)
import Console qualified
import Control.Monad qualified as Monad
import GHC.TypeLits qualified as GHC
import Mappable
import Maybe (Maybe)
import Record qualified
import Service.Event (InsertionType)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Thenable
import Uuid (Uuid)


data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


type family NameOf (t :: Type) :: Symbol


type family EntityOf (command :: Type) :: Type


class Command command where
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False


  type EntityIdType command :: Type
  type EntityIdType command = Uuid


  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType command)


  decideImpl :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))


type family GetEntityIdFunction (isTenant :: Bool) command id where
  GetEntityIdFunction False command id =
    command -> Maybe id
  GetEntityIdFunction True command id =
    Uuid -> command -> Maybe id


type family DecideFunction (isTenant :: Bool) command entity event where
  DecideFunction 'False command entity event =
    command -> Maybe entity -> Decision event
  DecideFunction 'True command entity event =
    Uuid -> command -> Maybe entity -> Decision event


type family EventOf (entityType :: Type)


type family ApiOf (commandType :: Type) :: Type


-- WEBAPI:MOVE INTO MODULE

data WebApi = WebApi


type ApiEndpointHandler = Bytes -> (Bytes -> Task Text Unit) -> Task Text Unit


class ApiBuilder api where
  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    api ->
    Record.Proxy command ->
    ApiEndpointHandler


instance ApiBuilder WebApi where
  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebApi ->
    Record.Proxy command ->
    ApiEndpointHandler
  buildCommandHandler _ _ body respond = do
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    Console.print n
    Console.print (body |> Text.fromBytes)
    respond (Text.toBytes n)


-- TODO: Replace Decision by a Task with context
data Decision a where
  Return :: a -> Decision a
  Bind :: Decision a -> (a -> Decision b) -> Decision b
  GenUuid :: Decision Uuid
  Accept :: InsertionType -> Array a -> Decision a
  Reject :: Text -> Decision a


instance Functor Decision where
  fmap f m = m |> Thenable.andThen (\r -> f r |> Return)


instance Applicative Decision where
  pure = Return
  (<*>) = Monad.ap


instance Monad Decision where
  m >>= f = Bind m f


-- Smart constructors

data DecisionContext = DecisionContext
  { genUuid :: Task Text Uuid
  }


runDecision :: (HasCallStack) => DecisionContext -> Decision a -> Task Text (CommandResult a)
runDecision ctx = go
 where
  go :: forall a. (HasCallStack) => Decision a -> Task Text (CommandResult a)
  go (Return _) = Task.throw "Decision didn't terminate with accept/reject"
  go (Bind m f) = case m of
    GenUuid -> do
      uuid <- ctx.genUuid
      f uuid |> go
    Accept _ _ -> Task.throw "Accept must be the last statement"
    Reject _ -> Task.throw "Reject must be the last statement"
    Return a -> go (f a)
    Bind m' f' -> go (Bind m' (\x -> Bind (f' x) f))
  go GenUuid = Task.throw "Unbound GenUuid"
  go (Accept s events) = AcceptCommand s events |> Task.yield
  go (Reject reason) = RejectCommand reason |> Task.yield