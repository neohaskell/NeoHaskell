module Service.OutboundIntegration.TH (
  deriveOutboundIntegration,
  outboundIntegration,
  outboundIntegrationFullEvent,
) where

import Control.Monad.Fail qualified as MonadFail
import Appendable ((++))
import Basics -- HOOK-ALLOW: Core reexports these markers, so TH must use the lower-level prelude.
import EventVariantOf (EventVariantOf)
import Maybe (Maybe (..))
import GHC.Base (String)
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Ppr qualified as THPpr
import Language.Haskell.TH.Syntax qualified as TH
import Service.CommandExecutor.TH (deriveKnownHash)


-- | Generate OutboundIntegration-related instances for a handler type.
--
-- Usage:
--
-- @
-- data ReserveStockOnItemAdded = ReserveStockOnItemAdded
--
-- type instance EntityOf ReserveStockOnItemAdded = CartEntity
--
-- handleEvent :: CartEntity -> ItemAddedPayload -> Integration.Outbound
-- handleEvent cart payload = Integration.batch [...]
--
-- deriveOutboundIntegration ''ReserveStockOnItemAdded
-- @
--
-- Validates at compile time:
--
-- * 'EntityOf' instance exists for the handler
-- * 'EventOf' resolves from the entity
-- * 'handleEvent' takes the entity and one concrete mapped payload
--
-- Generates:
--
-- * @instance OutboundIntegration HandlerType@ with 'HandledEvent' set
--   to the payload type
-- * @type instance NameOf HandlerType = "HandlerType"@
-- * @instance KnownHash "HandlerType"@
deriveOutboundIntegration :: TH.Name -> THLib.DecsQ
deriveOutboundIntegration handlerName = outboundIntegration handlerName
{-# INLINE deriveOutboundIntegration #-}


-- | Compatibility name for 'deriveOutboundIntegration'.
outboundIntegration :: TH.Name -> THLib.DecsQ
outboundIntegration handlerName = deriveHandler PayloadOnly handlerName


-- | Keep an existing full-event handler explicit while it is migrated.
-- The handler receives every constructor through the identity event mapping.
outboundIntegrationFullEvent :: TH.Name -> THLib.DecsQ
outboundIntegrationFullEvent handlerName = deriveHandler FullEvent handlerName


data HandlerMode = PayloadOnly | FullEvent


deriveHandler :: HandlerMode -> TH.Name -> THLib.DecsQ
deriveHandler mode handlerName = do
  let handlerNameStr = TH.nameBase handlerName

  entityTypeFamilyName <- lookupOrFail "EntityOf"
  entityTypeInstances <- TH.reifyInstances entityTypeFamilyName [TH.ConT handlerName]

  entityType <-
    case entityTypeInstances of
      [] ->
        MonadFail.fail
          [fmt|
ERROR: Missing EntityOf type instance for handler '#{handlerNameStr}'.

Please add: `type instance EntityOf #{handlerNameStr} = YourEntityType`
|]
      (TH.TySynInstD (TH.TySynEqn _ _ (TH.ConT name)) : _) -> pure name
      _ ->
        MonadFail.fail
          [fmt|
ERROR: EntityOf type instance for '#{handlerNameStr}' must be a concrete type.
|]

  let entityTypeStr = TH.nameBase entityType

  eventTypeFamilyName <- lookupOrFail "EventOf"
  eventTypeInstances <- TH.reifyInstances eventTypeFamilyName [TH.ConT entityType]

  eventType <-
    case eventTypeInstances of
      [] ->
        MonadFail.fail
          [fmt|
ERROR: Could not resolve EventOf for entity '#{entityTypeStr}'.

Ensure your entity has: `type instance EventOf #{entityTypeStr} = YourEventType`
|]
      (TH.TySynInstD (TH.TySynEqn _ _ (TH.ConT name)) : _) -> pure name
      _ ->
        MonadFail.fail
          [fmt|
ERROR: EventOf type instance for entity '#{entityTypeStr}' must be a concrete type.
|]

  handleEventName <-
    TH.lookupValueName "handleEvent"
      >>= orError
        [fmt|
ERROR: Missing 'handleEvent' function for handler '#{handlerNameStr}'.

Please add: `handleEvent :: #{entityTypeStr} -> YourPayload -> Integration.Outbound`
|]

  handleEventInfo <- TH.reify handleEventName
  payloadType <- case handleEventInfo of
    TH.VarI _ functionType _ -> do
      let prettyType = THPpr.pprint functionType
      case extractFunctionArgs functionType of
        Just (arg1, arg2) -> do
          let arg1Name = extractConName arg1
          let arg2Name = extractConName arg2
          if arg1Name != Just entityType
            then MonadFail.fail [fmt|ERROR: 'handleEvent' must take #{entityTypeStr} as its first argument. Current: #{prettyType}|]
            else case arg2Name of
              Nothing -> MonadFail.fail [fmt|ERROR: 'handleEvent' must take one concrete event payload as its second argument. Current: #{prettyType}|]
              Just _ -> do
                validatePayload mode eventType arg2
                pure arg2
        Nothing -> do
          MonadFail.fail
            [fmt|
ERROR: 'handleEvent' does not look like a two-argument function.

Expected: #{entityTypeStr} -> YourPayload -> Integration.Outbound
Current:  #{prettyType}
|]
    _ -> MonadFail.fail [fmt|ERROR: Could not inspect 'handleEvent' for handler #{handlerNameStr}.|]

  outboundIntegrationClassName <- lookupOrFail "OutboundIntegration"
  nameOfTypeFamilyName <- lookupOrFail "NameOf"

  let outboundInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT outboundIntegrationClassName `TH.AppT` TH.ConT handlerName)
          [ TH.TySynInstD
              ( TH.TySynEqn
                  Nothing
                  (TH.ConT (TH.mkName "HandledEvent") `TH.AppT` TH.ConT handlerName)
                  payloadType
              )
          , TH.ValD
              (TH.VarP (TH.mkName "handleEventImpl"))
              (TH.NormalB (TH.VarE handleEventName))
              []
          , TH.PragmaD (TH.InlineP (TH.mkName "handleEventImpl") TH.Inline TH.FunLike TH.AllPhases)
          ]

  let nameOfInstance =
        TH.TySynInstD
          ( TH.TySynEqn
              Nothing
              (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT handlerName)
              (TH.LitT (TH.StrTyLit handlerNameStr))
          )

  knownHashInstances <- deriveKnownHash handlerNameStr

  pure ([nameOfInstance, outboundInstance] ++ knownHashInstances)


validatePayload :: HandlerMode -> TH.Name -> TH.Type -> TH.Q ()
validatePayload mode eventType payloadType = do
  let eventTypeStr = TH.nameBase eventType
  let payloadTypeStr = THPpr.pprint payloadType
  case mode of
    FullEvent ->
      if extractConName payloadType == Just eventType
        then pure ()
        else MonadFail.fail [fmt|ERROR: outboundIntegrationFullEvent requires #{eventTypeStr} as the second handleEvent argument, but found #{payloadTypeStr}. Use outboundIntegration for a concrete payload.|]
    PayloadOnly -> do
      if extractConName payloadType == Just eventType
        then MonadFail.fail [fmt|ERROR: outboundIntegration requires one concrete event payload, but handleEvent takes the full #{eventTypeStr} event sum. Use outboundIntegrationFullEvent explicitly during migration.|]
        else pure ()
      mappings <- TH.reifyInstances ''EventVariantOf [TH.ConT eventType, payloadType]
      case mappings of
        [] -> MonadFail.fail [fmt|ERROR: No EventVariantOf #{eventTypeStr} #{payloadTypeStr} mapping exists. Generate it with deriveEntityEvents or define both fromVariant and toVariant before outboundIntegration.|]
        _ -> pure ()


-- | Look up a type name or fail with a helpful error.
lookupOrFail :: String -> TH.Q TH.Name
lookupOrFail name = do
  result <- TH.lookupTypeName name
  case result of
    Just n -> pure n
    Nothing ->
      MonadFail.fail
        [fmt|Could not find type: #{name}. Ensure you have `import Core` at the top of your module.|]


-- | Extract a value from Maybe or fail with an error message.
orError :: String -> Maybe value -> TH.Q value
orError errMsg mVal =
  case mVal of
    Just x -> pure x
    Nothing -> MonadFail.fail errMsg


-- | Extract the first two argument types from a function type AST.
--
-- A type @A -> B -> C@ is represented in TH as:
-- @AppT (AppT ArrowT A) (AppT (AppT ArrowT B) C)@
--
-- Returns @Just (A, B)@ for a two-or-more argument function, @Nothing@ otherwise.
extractFunctionArgs :: TH.Type -> Maybe (TH.Type, TH.Type)
extractFunctionArgs ty =
  case ty of
    TH.ForallT _ _ inner -> extractFunctionArgs inner
    TH.AppT (TH.AppT TH.ArrowT arg1) rest ->
      case rest of
        TH.AppT (TH.AppT TH.ArrowT arg2) _ -> Just (arg1, arg2)
        _ -> Nothing
    _ -> Nothing


-- | Extract the 'TH.Name' from a plain constructor type application.
--
-- Handles both bare @ConT name@ and applied types like @ConT name \`AppT\` ...@
-- by stripping any type applications and returning the outermost constructor name.
extractConName :: TH.Type -> Maybe TH.Name
extractConName ty =
  case ty of
    TH.ConT name -> Just name
    TH.AppT inner _ -> extractConName inner
    TH.SigT inner _ -> extractConName inner
    TH.ParensT inner -> extractConName inner
    _ -> Nothing
