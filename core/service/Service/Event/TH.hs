-- | Template Haskell markers for event payloads and entity event sums.
--
-- The 'deriveEvent' marker emits Show, Generic, Json.FromJSON, and Json.ToJSON
-- instances for the named type, skipping any that are already in scope.
--
-- Usage:
--
-- > data CartCreated = CartCreated { cartId :: Uuid }
-- >
-- > deriveEvent ''CartCreated
--
-- CONVENTION — \"marker last\":  Any custom instance you wish to write
-- (e.g. a hand-crafted 'Json.ToJSON') must be declared /before/ the
-- @deriveEvent@ call.  Declaring it after produces a GHC \"Duplicate instance\"
-- error because the marker will have already emitted the derived version.
module Service.Event.TH (
  deriveEvent,
  deriveEntityEvents,
  event,
) where

import Appendable ((++))
import Basics
import Control.Monad qualified as Monad
import Control.Monad.Fail qualified as MonadFail
import EventVariantOf qualified
import Json qualified
import LinkedList qualified
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Ppr qualified as THPpr
import Language.Haskell.TH.Syntax qualified as TH
import Maybe (Maybe (..))
import Service.Entity.Core qualified as Entity
import Service.TH.Boilerplate (emitJsonAndDerivingBoilerplate)
import Text qualified


-- | Derive Show, Generic, Json.FromJSON, and Json.ToJSON for an event type.
--
-- Each class is emitted only when it is not already in scope (idempotent).
deriveEvent :: TH.Name -> THLib.DecsQ
deriveEvent = emitJsonAndDerivingBoilerplate
{-# INLINE deriveEvent #-}


-- | Generate an entity's event sum from an explicit, nonempty payload list.
--
-- Each payload must already map to the entity. The generated sum routes
-- through payload 'Entity.Event' instances, which may follow the marker.
--
-- @
-- deriveEntityEvents ''Proposal [''ProposalCreated, ''PdfUploaded]
-- @
deriveEntityEvents :: TH.Name -> [TH.Name] -> THLib.DecsQ
deriveEntityEvents entityName payloadNames = do
  let entityBase = TH.nameBase entityName
  let entityText = Text.fromLinkedList entityBase
  let eventPrefix =
        if Text.endsWith "Entity" entityText
          then Text.dropRight 6 entityText
          else entityText
  let eventBase = Text.toLinkedList eventPrefix ++ "Event"
  let eventName = TH.mkName eventBase
  let constructorNames =
        payloadNames
          |> LinkedList.map (\payloadName -> TH.mkName (eventBase ++ TH.nameBase payloadName))
  validatePayloadNames entityName eventName payloadNames constructorNames
  validateAbsentEventMapping entityName
  Monad.mapM_ (validatePayloadOwnership entityName) payloadNames
  let constructors =
        LinkedList.map2
          (\constructorName payloadName -> TH.NormalC constructorName [(TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT payloadName)])
          constructorNames
          payloadNames
  let eventDeclaration =
        TH.DataD
          []
          eventName
          []
          Nothing
          constructors
          [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Show, TH.ConT ''Generic]]
  let eventOfDeclaration = familyDeclaration ''Entity.EventOf entityName eventName
  let entityOfDeclaration = familyDeclaration ''Entity.EntityOf eventName entityName
  let routingInstance = eventRouting eventName constructorNames
  let variantInstances = LinkedList.map2 (variantInstance eventName) constructorNames payloadNames
  let jsonOptions =
        TH.RecUpdE
          (TH.VarE 'Json.defaultOptions)
          [('Json.tagSingleConstructors, TH.ConE 'True)]
  let jsonInstances =
        [ TH.InstanceD
            Nothing
            []
            (TH.ConT ''Json.FromJSON `TH.AppT` TH.ConT eventName)
            [TH.FunD 'Json.parseJSON [TH.Clause [] (TH.NormalB (TH.VarE 'Json.genericParseJSON `TH.AppE` jsonOptions)) []]]
        , TH.InstanceD
            Nothing
            []
            (TH.ConT ''Json.ToJSON `TH.AppT` TH.ConT eventName)
            [TH.FunD 'Json.toJSON [TH.Clause [] (TH.NormalB (TH.VarE 'Json.genericToJSON `TH.AppE` jsonOptions)) []]]
        ]
  pure
    ( [eventDeclaration, eventOfDeclaration, entityOfDeclaration]
        ++ [routingInstance]
        ++ variantInstances
        ++ jsonInstances
    )


validatePayloadNames :: TH.Name -> TH.Name -> [TH.Name] -> [TH.Name] -> TH.Q ()
validatePayloadNames entityName eventName payloadNames constructorNames = do
  Monad.when (LinkedList.isEmpty payloadNames)
    (MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase entityName}' needs at least one event payload.|])
  Monad.when (hasDuplicate payloadNames)
    (MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase entityName}' lists an event payload twice. List each payload once.|])
  Monad.when (hasDuplicate constructorNames)
    (MonadFail.fail [fmt|deriveEntityEvents: two payloads for '#{TH.nameBase entityName}' would generate the same constructor. Rename one payload type.|])
  existingEvent <- TH.lookupTypeName (TH.nameBase eventName)
  case existingEvent of
    Just _ -> MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase eventName}' already exists. Choose an entity name with a distinct event type name.|]
    Nothing -> pure ()
  Monad.mapM_ validateConstructorName constructorNames
  Monad.mapM_ validatePayloadType payloadNames


hasDuplicate :: (Eq value) => [value] -> Bool
hasDuplicate values = case values of
  [] -> False
  value : remaining -> LinkedList.member value remaining || hasDuplicate remaining


validateConstructorName :: TH.Name -> TH.Q ()
validateConstructorName constructorName = do
  existing <- TH.lookupValueName (TH.nameBase constructorName)
  case existing of
    Just _ -> MonadFail.fail [fmt|deriveEntityEvents: constructor '#{TH.nameBase constructorName}' already exists. Rename the payload or its entity.|]
    Nothing -> pure ()


validatePayloadType :: TH.Name -> TH.Q ()
validatePayloadType payloadName = do
  information <- TH.reify payloadName
  case information of
    TH.TyConI (TH.DataD _ _ [] _ _ _) -> pure ()
    TH.TyConI (TH.NewtypeD _ _ [] _ _ _) -> pure ()
    _ -> MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase payloadName}' must be a concrete event payload type without type parameters.|]


validateAbsentEventMapping :: TH.Name -> TH.Q ()
validateAbsentEventMapping entityName = do
  existing <- TH.reifyInstances ''Entity.EventOf [TH.ConT entityName]
  case existing of
    [] -> pure ()
    _ -> MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase entityName}' already has an EventOf mapping. Remove the old event declaration or use a different entity type.|]


validatePayloadOwnership :: TH.Name -> TH.Name -> TH.Q ()
validatePayloadOwnership entityName payloadName = do
  existing <- TH.reifyInstances ''Entity.EntityOf [TH.ConT payloadName]
  case existing of
    [] -> MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase payloadName}' needs an EntityOf mapping to '#{TH.nameBase entityName}'. Define it before the marker.|]
    [TH.TySynInstD (TH.TySynEqn _ _ ownerType)] -> do
      resolvedOwner <- resolveNullaryAlias ownerType
      if resolvedOwner == TH.ConT entityName
        then pure ()
        else MonadFail.fail [fmt|deriveEntityEvents: '#{TH.nameBase payloadName}' has EntityOf '#{THPpr.pprint ownerType}', not '#{TH.nameBase entityName}'. Use payloads owned by the same entity.|]
    _ -> MonadFail.fail [fmt|deriveEntityEvents: cannot verify EntityOf for '#{TH.nameBase payloadName}'. Use a concrete entity type in that mapping.|]


resolveNullaryAlias :: TH.Type -> TH.Q TH.Type
resolveNullaryAlias typeValue = case typeValue of
  TH.ConT name -> do
    information <- TH.reify name
    case information of
      TH.TyConI (TH.TySynD _ [] expandedType) -> resolveNullaryAlias expandedType
      _ -> pure typeValue
  _ -> pure typeValue


familyDeclaration :: TH.Name -> TH.Name -> TH.Name -> TH.Dec
familyDeclaration familyName argumentName resultName =
  TH.TySynInstD
    (TH.TySynEqn Nothing (TH.ConT familyName `TH.AppT` TH.ConT argumentName) (TH.ConT resultName))


eventRouting :: TH.Name -> [TH.Name] -> TH.Dec
eventRouting eventName constructorNames = do
  let eventValue = TH.mkName "eventValue"
  let payloadValue = TH.mkName "payloadValue"
  let branches =
        constructorNames
          |> LinkedList.map (\constructorName ->
              TH.Match
                (TH.ConP constructorName [] [TH.VarP payloadValue])
                (TH.NormalB (TH.VarE 'Entity.getEventEntityIdImpl `TH.AppE` TH.VarE payloadValue))
                []
            )
  TH.InstanceD
    Nothing
    []
    (TH.ConT ''Entity.Event `TH.AppT` TH.ConT eventName)
    [ TH.FunD
        'Entity.getEventEntityIdImpl
        [TH.Clause [TH.VarP eventValue] (TH.NormalB (TH.CaseE (TH.VarE eventValue) branches)) []]
    ]


variantInstance :: TH.Name -> TH.Name -> TH.Name -> TH.Dec
variantInstance eventName constructorName payloadName = do
  let payloadValue = TH.mkName "payloadValue"
  let eventValue = TH.mkName "eventValue"
  TH.InstanceD
    Nothing
    []
    (TH.ConT ''EventVariantOf.EventVariantOf `TH.AppT` TH.ConT eventName `TH.AppT` TH.ConT payloadName)
    [ TH.FunD
        'EventVariantOf.fromVariant
        [TH.Clause [TH.VarP payloadValue] (TH.NormalB (TH.ConE constructorName `TH.AppE` TH.VarE payloadValue)) []]
    , TH.FunD
        'EventVariantOf.toVariant
        [ TH.Clause
            [TH.VarP eventValue]
            ( TH.NormalB
                ( TH.CaseE
                    (TH.VarE eventValue)
                    [ TH.Match
                        (TH.ConP constructorName [] [TH.VarP payloadValue])
                        (TH.NormalB (TH.ConE 'Just `TH.AppE` TH.VarE payloadValue))
                        []
                    , TH.Match TH.WildP (TH.NormalB (TH.ConE 'Nothing)) []
                    ]
                )
            )
            []
        ]
    ]


-- | Compatibility name for 'deriveEvent'.
event :: TH.Name -> THLib.DecsQ
event = deriveEvent
{-# INLINE event #-}
