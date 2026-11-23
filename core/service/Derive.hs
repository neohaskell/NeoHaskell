-- | Derive framework instances from application records and their domain hooks.
--
-- Import this module alongside Core in a declaration module. Put command and
-- query markers after their hooks. Put the entity marker before its reducer so
-- the reducer can pattern-match on the generated event constructors.
module Derive (
  derive,
  eventPayload,
  entity,
  EntityOptions (events, eventIds),
  command,
  CommandOptions (entity, transports),
  query,
  QueryOptions (entities, projectionHooks),
) where

import Appendable ((++))
import Basics
import Control.Monad qualified as Monad
import Control.Monad.Fail qualified as MonadFail
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList qualified
import Maybe (Maybe (..))
import Service.Command.Core qualified as Command
import Service.CommandExecutor.TH qualified as CommandTH
import Service.Entity.Core qualified as Entity
import Service.Entity.TH qualified as EntityTH
import Service.Event.TH qualified as EventTH
import Service.Query.Core qualified as Query
import Service.Query.TH qualified as QueryTH


class Declaration options where
  -- | Derive the selected role for the quoted application type.
  derive :: TH.Name -> options -> THLib.DecsQ


data EventPayload = EventPayload


-- | Generate an event payload's display and JSON instances.
eventPayload :: EventPayload
eventPayload = EventPayload


instance Declaration EventPayload where
  derive name EventPayload = EventTH.deriveEvent name


data EntityOptions = EntityOptions
  { events :: [TH.Name],
    eventIds :: [(TH.Name, TH.Name)]
  }


-- | Select the entity role. List its owned payloads in @events@. Each payload
-- routes by its @entityId@ field unless @eventIds@ supplies a named extractor.
entity :: EntityOptions
entity = EntityOptions {events = [], eventIds = []}


instance Declaration EntityOptions where
  derive name options = do
    eventDeclarations <- EventTH.deriveOwnedEvents name options.events options.eventIds
    stateDeclarations <- EntityTH.deriveEntityState name
    pure (eventDeclarations ++ stateDeclarations)


data CommandOptions = CommandOptions
  { entity :: TH.Name,
    transports :: [TH.Name]
  }


-- | Select the command role. Set @entity@ to its owner. The default transport
-- selection resolves to the application's sole public adapter at startup.
command :: CommandOptions
command = CommandOptions {entity = TH.mkName "", transports = [''Command.InferredTransport]}


instance Declaration CommandOptions where
  derive name options = do
    Monad.when
      (options.entity == TH.mkName "")
      (MonadFail.fail [fmt|derive: command '#{TH.nameBase name}' needs an entity. Set entity = ''YourEntity.|])
    Monad.when
      (LinkedList.isEmpty options.transports)
      ( MonadFail.fail
          [fmt|derive: command '#{TH.nameBase name}' has an empty transports list. Omit transports to infer the adapter, or select its transports explicitly.|]
      )
    declarations <- CommandTH.deriveCommandFor name options.entity
    let typeList =
          LinkedList.foldr
            (\transport rest -> TH.PromotedConsT `TH.AppT` TH.ConT transport `TH.AppT` rest)
            TH.PromotedNilT
            options.transports
        mapping familyType result = TH.TySynInstD (TH.TySynEqn Nothing (TH.ConT familyType `TH.AppT` TH.ConT name) result)
    pure (mapping ''Entity.EntityOf (TH.ConT options.entity) : mapping ''Command.TransportsOf typeList : declarations)


data QueryOptions = QueryOptions
  { entities :: [TH.Name],
    projectionHooks :: [(TH.Name, TH.Name, TH.Name)]
  }


-- | Select the query role. For each contributing entity, supply
-- @projectionIdFromEntityName@ and @combineEntityName@. Override the pair
-- with @(entityType, projectionIdFunction, combineFunction)@ in @projectionHooks@.
query :: QueryOptions
query = QueryOptions {entities = [], projectionHooks = []}


instance Declaration QueryOptions where
  derive name options = do
    Monad.when
      (LinkedList.isEmpty options.entities)
      (MonadFail.fail [fmt|derive: query '#{TH.nameBase name}' needs at least one source in entities.|])
    Monad.mapM_
      ( \(source, _, _) ->
          Monad.unless
            (LinkedList.member source options.entities)
            (MonadFail.fail [fmt|derive: projection hook for unlisted source '#{TH.nameBase source}'.|])
      )
      options.projectionHooks
    queryDeclarations <- QueryTH.deriveQuery name options.entities
    projections <- Monad.mapM (projectionInstance name options.projectionHooks) options.entities
    pure (queryDeclarations ++ projections)


projectionInstance :: TH.Name -> [(TH.Name, TH.Name, TH.Name)] -> TH.Name -> TH.Q TH.Dec
projectionInstance name overrides source = do
  (getId, combine) <- case LinkedList.filter (\(target, _, _) -> target == source) overrides of
    [(_, idHook, combineHook)] -> pure (idHook, combineHook)
    [] -> do
      let required prefix = do
            let hook = prefix ++ TH.nameBase source
            found <- TH.lookupValueName hook
            case found of
              Nothing -> MonadFail.fail [fmt|derive: query '#{TH.nameBase name}' needs '#{hook}' for '#{TH.nameBase source}'.|]
              Just resolved -> pure resolved
      idHook <- required "projectionIdFrom"
      combineHook <- required "combine"
      pure (idHook, combineHook)
    _ -> MonadFail.fail [fmt|derive: multiple projection hooks for '#{TH.nameBase source}'.|]
  let method which hook = TH.FunD which [TH.Clause [] (TH.NormalB (TH.VarE hook)) []]
  pure
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''Query.QueryOf `TH.AppT` TH.ConT source `TH.AppT` TH.ConT name)
        [method 'Query.queryId getId, method 'Query.combine combine]
    )
