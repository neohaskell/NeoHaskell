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

  -- type instance EntityOf CreateCart = Entity
  entityOfName <- TH.lookupTypeName "EntityOf" >>= orError "FIXME: EntityOf type family not found"
  let entityTypeInstance =
        TH.TySynInstD
          (TH.TySynEqn Nothing (TH.ConT entityOfName `TH.AppT` TH.ConT someName) (TH.ConT entityType))

  -- instance Command CreateCart where
  --   type IsMultiTenant CreateCart = MultiTenancy (if MultiTenancy type exists)
  --   getEntityIdImpl = getEntityId
  --   decideImpl = decide
  maybeMultiTenancy <- TH.lookupTypeName "MultiTenancy"
  commandClassName <- TH.lookupTypeName "Command" >>= orError "FIXME: Command type class not found"

  let multiTenancyDecl = case maybeMultiTenancy of
        Just multiTenancyType ->
          [ TH.TySynInstD
              (TH.TySynEqn Nothing (TH.ConT (TH.mkName "IsMultiTenant") `TH.AppT` TH.ConT someName) (TH.ConT multiTenancyType))
          ]
        Nothing -> []

  let commandInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT commandClassName `TH.AppT` TH.ConT someName)
          ( multiTenancyDecl
              ++ [ TH.ValD (TH.VarP (TH.mkName "getEntityIdImpl")) (TH.NormalB (TH.VarE getEntityId)) [],
                   TH.ValD (TH.VarP (TH.mkName "decideImpl")) (TH.NormalB (TH.VarE decide)) []
                 ]
          )

  pure [entityTypeInstance, commandInstance]
{-# INLINE deriveCommand #-}