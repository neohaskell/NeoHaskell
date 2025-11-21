{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
  deriveCommand,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.List qualified as GhcList
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Ppr qualified as THPpr
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

  -- Validate function signatures when MultiTenancy is True
  case maybeMultiTenancy of
    Just multiTenancyName -> do
      multiTenancyInfo <- TH.reify multiTenancyName
      let isMultiTenancyTrue = case multiTenancyInfo of
            TH.TyConI (TH.TySynD _ _ (TH.PromotedT trueName)) ->
              TH.nameBase trueName == "True"
            _ -> False
      if isMultiTenancyTrue
        then do
          getEntityIdInfo <- TH.reify getEntityId
          decideInfo <- TH.reify decide
          case getEntityIdInfo of
            TH.VarI _ getEntityIdType _ -> do
              let typeStr = THPpr.pprint getEntityIdType
              case typeStr of
                str | "Uuid.Uuid" `GhcList.elem` GhcList.words str -> pure ()
                _ ->
                  MonadFail.fail
                    [fmt|
ERROR: When MultiTenancy is set to True, 'getEntityId' must have Uuid as first parameter.

Expected signature:
  getEntityId :: Uuid -> #{TH.nameBase someName} -> Maybe Text

Current signature doesn't include Uuid as first parameter.

Please update your getEntityId function to accept a Uuid tenant ID as the first argument.
|]
            _ -> pure ()
          case decideInfo of
            TH.VarI _ decideType _ -> do
              let typeStr = show decideType
              case typeStr of
                str | "Uuid.Uuid" `GhcList.elem` GhcList.words str -> pure ()
                _ ->
                  MonadFail.fail
                    [fmt|
ERROR: When MultiTenancy is set to True, 'decide' must have Uuid as first parameter.

Expected signature:
  decide :: Uuid -> #{TH.nameBase someName} -> Maybe #{TH.nameBase entityType} -> Decision event

Current signature doesn't include Uuid as first parameter.

Please update your decide function to accept a Uuid tenant ID as the first argument.
|]
            _ -> pure ()
        else pure ()
    Nothing -> pure ()

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