{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
  deriveCommand,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Maybe qualified
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event (EntityName)
import Service.EventStore.Core (EventStore)
import Task qualified


-- | Result of executing a command through the CommandHandler
data CommandHandlerResult
  = CommandAccepted
      { streamId :: StreamId,
        eventsAppended :: Int,
        retriesAttempted :: Int
      }
  | CommandRejected
      { reason :: Text
      }
  | CommandFailed
      { error :: Text,
        retriesAttempted :: Int
      }
  deriving (Eq, Show, Ord, Generic)


data CommandHandler event = CommandHandler
  { eventStore :: EventStore event,
    maxRetries :: Int,
    retryDelayMs :: Int
  }
  deriving (Generic)


execute ::
  forall command commandEntity commandEvent appEvent error.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity
  ) =>
  EventStore appEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  command ->
  Task error CommandHandlerResult
execute _eventStore _entityFetcher _entityName _command = do
  Task.yield (panic "Service.CommandHandler.execute not yet implemented")


deriveCommand :: TH.Name -> THLib.DecsQ
deriveCommand someName = do
  let orError errMsg mVal =
        case mVal of
          Just x -> pure x
          Nothing -> MonadFail.fail errMsg
  maybeEntityType <- TH.lookupTypeName "Entity"
  maybeGetEntityId <- TH.lookupValueName "getEntityId"
  maybeDecide <- TH.lookupValueName "decide"
  entityType <- maybeEntityType |> orError "FIXME: This module doesn't have an Entity type"
  getEntityId <- maybeGetEntityId |> orError "FIXME: This module doesn't have a getEntityId function"
  decide <- maybeDecide |> orError "FIXME: This module doesn't have a decide function"
  MonadFail.fail "Undefined"
{-# INLINE deriveCommand #-}