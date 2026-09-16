-- | Derive entity plumbing while keeping its state transition in application code.
--
-- Define @initialState@, @update@, and @getEventEntityId@ before calling
-- @deriveEntity ''CartEntity ''CartEvent@. Existing instances take precedence;
-- companions are required only for the corresponding missing business instances.
-- A missing 'Default.Default' delegates to the entity's initial state.
--
-- Unlike event and command derivation, entity derivation does not require Show.
module Service.Entity.TH (deriveEntity) where

import Appendable ((++))
import Basics (Generic, (==), fmt, pure) -- HOOK-ALLOW: TH.Q compiler support below Core; Task cannot replace this Applicative.
import Char (Char)
import Control.Monad.Fail qualified as MonadFail -- HOOK-ALLOW: TH.Q reports compile-time errors through MonadFail; not a runtime Task.
import Default qualified
import Json qualified
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Maybe (Maybe (..))
import Service.Command.Core qualified as Command
import Service.Entity.Core qualified as Entity
import Service.TH.Boilerplate (emitEmptyInstance, emitInstanceIfMissing, emitStockDeriving)


-- | Derive Generic, JSON, naming, entity/event associations, Default, Entity,
-- and event routing. Existing custom names, encodings, identifier types, and
-- instances are preserved. Incompatible event/entity associations fail early.
deriveEntity :: TH.Name -> TH.Name -> THLib.DecsQ
deriveEntity entityName eventName = do
  nameDeclarations <- emitNameIfMissing entityName
  eventOfDeclarations <- emitCheckedFamily ''Entity.EventOf entityName eventName
  entityOfDeclarations <- emitCheckedFamily ''Entity.EntityOf eventName entityName
  genericDeclarations <- emitStockDeriving ''Generic entityName
  fromJsonDeclarations <- emitEmptyInstance ''Json.FromJSON entityName
  toJsonDeclarations <- emitEmptyInstance ''Json.ToJSON entityName
  entityDeclarations <-
    emitInstanceIfMissing ''Entity.Entity entityName (emitEntityInstance entityName)
  defaultDeclarations <-
    emitInstanceIfMissing ''Default.Default entityName (emitDefaultInstance entityName)
  routingDeclarations <-
    emitInstanceIfMissing ''Entity.Event eventName (emitRoutingInstance eventName)
  pure -- HOOK-ALLOW: return compiler declarations in TH.Q, not a runtime Task.
    ( nameDeclarations
        ++ eventOfDeclarations
        ++ entityOfDeclarations
        ++ genericDeclarations
        ++ fromJsonDeclarations
        ++ toJsonDeclarations
        ++ entityDeclarations
        ++ defaultDeclarations
        ++ routingDeclarations
    )


emitNameIfMissing :: TH.Name -> THLib.DecsQ
emitNameIfMissing entityName = do
  existing <- TH.reifyInstances ''Command.NameOf [TH.ConT entityName]
  case existing of
    [] -> do
      let nameValue = TH.LitT (TH.StrTyLit (TH.nameBase entityName))
      pure [familyDeclaration ''Command.NameOf entityName nameValue] -- HOOK-ALLOW: TH.Q declaration generation.
    _ -> pure [] -- HOOK-ALLOW: TH.Q declaration generation.


emitCheckedFamily :: TH.Name -> TH.Name -> TH.Name -> THLib.DecsQ
emitCheckedFamily familyName argumentName expectedName = do
  existing <- TH.reifyInstances familyName [TH.ConT argumentName]
  case existing of
    [] ->
      pure [familyDeclaration familyName argumentName (TH.ConT expectedName)] -- HOOK-ALLOW: TH.Q declaration generation.
    [TH.TySynInstD (TH.TySynEqn _ _ existingType)] ->
      checkExistingMapping familyName argumentName expectedName existingType
    _ -> conflictingFamily familyName argumentName expectedName


checkExistingMapping :: TH.Name -> TH.Name -> TH.Name -> TH.Type -> THLib.DecsQ
checkExistingMapping familyName argumentName expectedName existingType = do
  actual <- resolveNullaryAliases existingType
  expected <- resolveNullaryAliases (TH.ConT expectedName)
  if actual == expected
    then pure [] -- HOOK-ALLOW: TH.Q declaration generation.
    else conflictingFamily familyName argumentName expectedName


familyDeclaration :: TH.Name -> TH.Name -> TH.Type -> TH.Dec
familyDeclaration familyName argumentName resultType =
  TH.TySynInstD
    ( TH.TySynEqn
        Nothing
        (TH.ConT familyName `TH.AppT` TH.ConT argumentName)
        resultType
    )


conflictingFamily :: TH.Name -> TH.Name -> TH.Name -> THLib.DecsQ
conflictingFamily familyName argumentName expectedName =
  MonadFail.fail
    [fmt|deriveEntity: existing #{TH.nameBase familyName} mapping for '#{TH.nameBase argumentName}' must resolve to '#{TH.nameBase expectedName}'. Make the marker arguments and the existing type-family equation agree.|]


-- Only expand ordinary nullary synonyms; do not attempt to evaluate arbitrary
-- type-family applications. GHC already rejects cyclic type synonyms.
resolveNullaryAliases :: TH.Type -> TH.Q TH.Type
resolveNullaryAliases originalType =
  case originalType of
    TH.ConT name -> do
      information <- TH.reify name
      resolveReifiedAlias originalType information
    _ -> pure originalType -- HOOK-ALLOW: TH.Q type inspection.


resolveReifiedAlias :: TH.Type -> TH.Info -> TH.Q TH.Type
resolveReifiedAlias originalType information =
  case information of
    TH.TyConI (TH.TySynD _ [] expandedType) -> resolveNullaryAliases expandedType
    _ -> pure originalType -- HOOK-ALLOW: TH.Q type inspection.


emitEntityInstance :: TH.Name -> THLib.DecsQ
emitEntityInstance entityName = do
  initialStateName <- lookupCompanion entityName "initialState"
  updateName <- lookupCompanion entityName "update"
  let methods =
        [ methodDeclaration 'Entity.initialStateImpl initialStateName
        , methodDeclaration 'Entity.updateImpl updateName
        ]
  pure [instanceDeclaration ''Entity.Entity entityName methods] -- HOOK-ALLOW: TH.Q declaration generation.


emitDefaultInstance :: TH.Name -> THLib.DecsQ
emitDefaultInstance entityName = do
  let methods = [methodDeclaration 'Default.def 'Entity.initialStateImpl]
  pure [instanceDeclaration ''Default.Default entityName methods] -- HOOK-ALLOW: TH.Q declaration generation.


emitRoutingInstance :: TH.Name -> THLib.DecsQ
emitRoutingInstance eventName = do
  routingName <- lookupCompanion eventName "getEventEntityId"
  let methods = [methodDeclaration 'Entity.getEventEntityIdImpl routingName]
  pure [instanceDeclaration ''Entity.Event eventName methods] -- HOOK-ALLOW: TH.Q declaration generation.


lookupCompanion :: TH.Name -> [Char] -> TH.Q TH.Name
lookupCompanion typeName companion = do
  found <- TH.lookupValueName companion
  case found of
    Just name -> pure name -- HOOK-ALLOW: TH.Q name lookup.
    Nothing ->
      MonadFail.fail
        [fmt|deriveEntity: missing '#{companion}' companion for '#{TH.nameBase typeName}'. Define the application function before the deriveEntity marker, or provide the corresponding Entity/Event instance before it. The marker does not invent state transitions or routing rules.|]


instanceDeclaration :: TH.Name -> TH.Name -> [TH.Dec] -> TH.Dec
instanceDeclaration className typeName methods =
  TH.InstanceD
    Nothing
    []
    (TH.ConT className `TH.AppT` TH.ConT typeName)
    methods


methodDeclaration :: TH.Name -> TH.Name -> TH.Dec
methodDeclaration methodName implementationName =
  TH.FunD methodName [TH.Clause [] (TH.NormalB (TH.VarE implementationName)) []]
