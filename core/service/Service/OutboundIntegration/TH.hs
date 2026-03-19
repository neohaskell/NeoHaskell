module Service.OutboundIntegration.TH (
  outboundIntegration,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.List qualified as GhcList
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

  -- Step 4: Validate handleEvent signature
  handleEventInfo <- TH.reify handleEventName
  case handleEventInfo of
    TH.VarI _ functionType _ -> do
      let typeStr = THPpr.pprint functionType
      let normalizedType = normalizeTypeString typeStr
      let hasEntityType = entityTypeStr `isInfixOf` normalizedType
      let hasEventType = eventTypeStr `isInfixOf` normalizedType
      if not hasEntityType || not hasEventType
        then do
          let expectedSig = entityTypeStr ++ " -> " ++ eventTypeStr ++ " -> Outbound"
          MonadFail.fail
            [fmt|
ERROR: 'handleEvent' has incorrect signature.

Expected: #{expectedSig}
Current:  #{normalizedType}
|]
        else pure ()
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


-- | Normalize a type string by removing module qualifiers.
normalizeTypeString :: String -> String
normalizeTypeString t =
  t
    |> GhcList.words
    |> GhcList.map (\w -> w |> splitOn "." |> GhcList.last)
    |> GhcList.unwords


-- | Split a string by a delimiter.
splitOn :: String -> String -> [String]
splitOn delimiter str = do
  let go acc current remaining =
        case remaining of
          [] -> GhcList.reverse (GhcList.reverse current : acc)
          c : rest -> do
            if GhcList.take (GhcList.length delimiter) remaining == delimiter
              then go (GhcList.reverse current : acc) [] (GhcList.drop (GhcList.length delimiter) remaining)
              else go acc (c : current) rest
  go [] [] str


-- | Check if a string is an infix of another.
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = do
  let go n h =
        case n of
          [] -> True
          _ ->
            case h of
              [] -> False
              _ : hs ->
                (GhcList.take (GhcList.length n) h == n) || go n hs
  go needle haystack
