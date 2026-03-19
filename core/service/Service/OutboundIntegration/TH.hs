module Service.OutboundIntegration.TH (
  outboundIntegration,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
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
--   deriving (Generic, Typeable, Show)
--
-- type instance EntityOf ReserveStockOnItemAdded = CartEntity
--
-- handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
-- handleEvent cart event = case event of
--   ItemAdded {stockId, quantity} -> Integration.batch [...]
--   _ -> Integration.none
--
-- outboundIntegration ''ReserveStockOnItemAdded
-- @
--
-- Validates at compile time:
--
-- * 'EntityOf' instance exists for the handler
-- * 'EventOf' resolves from the entity
-- * 'handleEvent' function exists with correct signature:
--   @EntityType -> EventType -> Integration.Outbound@
--
-- Generates:
--
-- * @instance OutboundIntegration HandlerType@ with 'HandledEvent' set
--   to the event ADT type (EventOf entity)
-- * @type instance NameOf HandlerType = "HandlerType"@
-- * @instance KnownHash "HandlerType"@
outboundIntegration :: TH.Name -> THLib.DecsQ
outboundIntegration handlerName = do
  let handlerNameStr = TH.nameBase handlerName

  -- Step 1: Look up EntityOf
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

  -- Step 2: Look up EventOf
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

  let eventTypeStr = TH.nameBase eventType

  -- Step 3: Look up handleEvent
  handleEventName <-
    TH.lookupValueName "handleEvent"
      >>= orError
        [fmt|
ERROR: Missing 'handleEvent' function for handler '#{handlerNameStr}'.

Please add: `handleEvent :: #{entityTypeStr} -> #{eventTypeStr} -> Integration.Outbound`
|]

  -- Step 4: Validate handleEvent signature structurally.
  -- Extract the first two argument types from the function type AST and
  -- compare them directly as TH.Name values — no string manipulation.
  handleEventInfo <- TH.reify handleEventName
  case handleEventInfo of
    TH.VarI _ functionType _ -> do
      let prettyType = THPpr.pprint functionType
      case extractFunctionArgs functionType of
        Just (arg1, arg2) -> do
          let arg1Name = extractConName arg1
          let arg2Name = extractConName arg2
          let entityOk = arg1Name == Just entityType
          let eventOk = arg2Name == Just eventType
          if not entityOk || not eventOk
            then do
              let expectedSig = entityTypeStr ++ " -> " ++ eventTypeStr ++ " -> Outbound"
              MonadFail.fail
                [fmt|
ERROR: 'handleEvent' has incorrect signature.

Expected: #{expectedSig}
Current:  #{prettyType}
|]
            else pure ()
        Nothing -> do
          let expectedSig = entityTypeStr ++ " -> " ++ eventTypeStr ++ " -> Outbound"
          MonadFail.fail
            [fmt|
ERROR: 'handleEvent' does not look like a two-argument function.

Expected: #{expectedSig}
Current:  #{prettyType}
|]
    _ -> pure ()

  -- Step 5: Look up required type names
  outboundIntegrationClassName <- lookupOrFail "OutboundIntegration"
  nameOfTypeFamilyName <- lookupOrFail "NameOf"

  -- Step 6: Generate OutboundIntegration instance
  let outboundInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT outboundIntegrationClassName `TH.AppT` TH.ConT handlerName)
          [ TH.TySynInstD
              ( TH.TySynEqn
                  Nothing
                  (TH.ConT (TH.mkName "HandledEvent") `TH.AppT` TH.ConT handlerName)
                  (TH.ConT eventType)
              )
          , TH.ValD
              (TH.VarP (TH.mkName "handleEventImpl"))
              (TH.NormalB (TH.VarE handleEventName))
              []
          , TH.PragmaD (TH.InlineP (TH.mkName "handleEventImpl") TH.Inline TH.FunLike TH.AllPhases)
          ]

  -- Step 7: Generate NameOf type instance
  let nameOfInstance =
        TH.TySynInstD
          ( TH.TySynEqn
              Nothing
              (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT handlerName)
              (TH.LitT (TH.StrTyLit handlerNameStr))
          )

  -- Step 8: Generate KnownHash instance
  knownHashInstances <- deriveKnownHash handlerNameStr

  pure ([nameOfInstance, outboundInstance] ++ knownHashInstances)


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
