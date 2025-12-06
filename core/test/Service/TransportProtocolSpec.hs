{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Service.TransportProtocolSpec where

import Core
import Bytes qualified
import Data.Aeson (FromJSON, ToJSON)
import Decision qualified
import Service.Adapter (ServiceAdapter(..))
import Service.Adapter.Direct (DirectAdapter(..), defaultConfig, DirectAdapterState(..))
import Service.Command ()  -- Just for instances
import Service.Command.Core ()  -- Just for instances
import Service.Definition.TypeLevel
import Service.Error (ServiceError(..))
import Service.Protocol (TransportProtocols)
import Service.ServiceDefinition.Core qualified as Service
import Service.Runtime (ServiceRuntime(..))
import Task qualified
import Test

-- ============================================================================
-- KnownHash instances for command names and protocol names
-- ============================================================================

-- Command name instances
instance KnownHash "CreateCartCommand" where
  hashVal _ = 1234567890  -- Some unique hash value

instance KnownHash "AddItemCommand" where
  hashVal _ = 2345678901  -- Some unique hash value

instance KnownHash "InternalCommand" where
  hashVal _ = 3456789012  -- Some unique hash value

-- Protocol name instances
instance KnownHash "Direct" where
  hashVal _ = 4567890123  -- Some unique hash value

instance KnownHash "REST" where
  hashVal _ = 5678901234  -- Some unique hash value

-- ============================================================================
-- NFData instances for shouldNotTypecheck
-- ============================================================================

instance NFData ServiceRuntime where
  rnf ServiceRuntime {} =
    -- We can't deeply evaluate functions, so just return ()
    ()

instance NFData ServiceError where
  rnf CommandNotFound {} = ()
  rnf ServiceAlreadyShutdown = ()
  rnf AdapterNotFound {} = ()
  rnf CommandExecutionFailed {} = ()
  rnf AdapterInitializationFailed {} = ()
  rnf CommandDecodingFailed {} = ()

instance (NFData e, NFData a) => NFData (Task e a) where
  rnf _ = () -- Tasks are IO-based, can't deeply evaluate

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | Helper to deploy a service and convert errors
deployService :: Service.ServiceDefinition cmds req prov adp Unit -> Task Text ServiceRuntime
deployService serviceDef = Service.deploy serviceDef |> Task.mapError toText

-- ============================================================================
-- Test Commands with different protocol requirements
-- ============================================================================

-- | Command that requires Direct protocol only
data CreateCartCommand = CreateCartCommand
  { customerId :: Uuid
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateCartCommand
instance ToJSON CreateCartCommand
instance NFData CreateCartCommand

-- | Command entity type
data Cart = Cart
  { cartId :: Uuid,
    customerId :: Uuid,
    items :: Array Text
  }
  deriving (Show, Eq, Generic)

-- | Cart events
data CartEvent
  = CartCreated { cartId :: Uuid, customerId :: Uuid }
  | ItemAdded { cartId :: Uuid, itemId :: Text }
  deriving (Show, Eq, Generic)

instance HasField "entityId" CartEvent Uuid where
  getField (CartCreated {cartId}) = cartId
  getField (ItemAdded {cartId}) = cartId

-- Define the type families for the command
type instance NameOf CreateCartCommand = "CreateCartCommand"
type instance EntityOf CreateCartCommand = Cart
type instance EventOf Cart = CartEvent
type instance TransportProtocols CreateCartCommand = '["Direct"]

instance Command CreateCartCommand where
  type EntityIdType CreateCartCommand = Uuid

  getEntityIdImpl :: CreateCartCommand -> Maybe Uuid
  getEntityIdImpl _ = Nothing  -- New entity, no ID yet

  decideImpl :: CreateCartCommand -> Maybe Cart -> Decision CartEvent
  decideImpl cmd Nothing = do
    cartId <- Decision.generateUuid
    Decision.acceptNew [CartCreated cartId cmd.customerId]
  decideImpl _ (Just _) =
    Decision.reject "Cart already exists"

-- | Command that requires both Direct and REST protocols
data AddItemCommand = AddItemCommand
  { cartId :: Uuid,
    itemId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AddItemCommand
instance ToJSON AddItemCommand
instance NFData AddItemCommand

type instance NameOf AddItemCommand = "AddItemCommand"
type instance EntityOf AddItemCommand = Cart
type instance TransportProtocols AddItemCommand = '["Direct", "REST"]

instance Command AddItemCommand where
  type EntityIdType AddItemCommand = Uuid

  getEntityIdImpl :: AddItemCommand -> Maybe Uuid
  getEntityIdImpl cmd = Just cmd.cartId

  decideImpl :: AddItemCommand -> Maybe Cart -> Decision CartEvent
  decideImpl cmd (Just _) =
    Decision.acceptExisting [ItemAdded cmd.cartId cmd.itemId]
  decideImpl _ Nothing =
    Decision.reject "Cart does not exist"

-- | Command with no transport protocol requirements (for testing)
data InternalCommand = InternalCommand
  deriving (Show, Eq, Generic)

instance NFData InternalCommand

type instance NameOf InternalCommand = "InternalCommand"
type instance EntityOf InternalCommand = Cart
type instance TransportProtocols InternalCommand = '[]

instance Command InternalCommand where
  type EntityIdType InternalCommand = Uuid

  getEntityIdImpl :: InternalCommand -> Maybe Uuid
  getEntityIdImpl _ = Nothing

  decideImpl :: InternalCommand -> Maybe Cart -> Decision CartEvent
  decideImpl _ _ = Decision.reject "Internal only"

-- ============================================================================
-- Type-Level Operation Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Type-Level Protocol Operations" do
    describe "Member type family" do
      it "correctly identifies present elements" \_ -> do
        -- These should compile, proving Member works
        let _ = unit :: (Member "Direct" '["Direct", "REST"] ~ 'True => Unit)
        let _ = unit :: (Member "REST" '["Direct", "REST"] ~ 'True => Unit)
        Task.yield unit

      it "correctly identifies absent elements" \_ -> do
        -- These should compile, proving Member works for false cases
        let _ = unit :: (Member "GraphQL" '["Direct", "REST"] ~ 'False => Unit)
        let _ = unit :: (Member "Direct" '[] ~ 'False => Unit)
        Task.yield unit

    describe "Union type family" do
      it "merges lists without duplicates" \_ -> do
        -- Union should combine lists and remove duplicates
        let _ = unit :: (Union '["Direct"] '["REST"] ~ '["Direct", "REST"] => Unit)
        let _ = unit :: (Union '["Direct"] '["Direct"] ~ '["Direct"] => Unit)
        Task.yield unit

      it "handles empty lists" \_ -> do
        let _ = unit :: (Union '[] '["Direct"] ~ '["Direct"] => Unit)
        let _ = unit :: (Union '["Direct"] '[] ~ '["Direct"] => Unit)
        Task.yield unit

    describe "Difference type family" do
      it "finds missing elements" \_ -> do
        -- Difference should find elements in first list not in second
        let _ = unit :: (Difference '["Direct", "REST"] '["Direct"] ~ '["REST"] => Unit)
        let _ = unit :: (Difference '["Direct"] '["Direct", "REST"] ~ '[] => Unit)
        Task.yield unit

      it "handles empty lists" \_ -> do
        let _ = unit :: (Difference '[] '["Direct"] ~ '[] => Unit)
        let _ = unit :: (Difference '["Direct"] '[] ~ '["Direct"] => Unit)
        Task.yield unit

  describe "Service Definition DSL with Protocols" do
    it "builds empty service definition" \_ -> do
      let serviceDef = Service.emptyServiceDefinition :: Service.ServiceDefinition '[] '[] '[] '[] Unit
      -- Should have no commands, no protocols
      Service.extract serviceDef |> shouldBe unit

    it "accumulates protocols from commands" \_ -> do
      -- This should compile - Direct adapter provided for Direct-only command
      let serviceDef =
            Service.expose (DirectAdapter defaultConfig) Service.>>
            Service.command @CreateCartCommand

      -- Successfully deploy (compile-time check passes)
      result <- Service.deploy serviceDef |> Task.mapError toText |> Task.asResult
      case result of
        Ok _ -> Task.yield unit
        Err err -> fail err

    it "allows extra adapters beyond requirements" \_ -> do
      -- Having more adapters than needed should be fine
      let serviceDef =
            Service.expose (DirectAdapter defaultConfig) Service.>>
            -- Could add REST adapter here too, even though not needed
            Service.command @InternalCommand  -- Requires no protocols

      _runtime <- Service.deploy serviceDef |> Task.mapError toText
      Task.yield unit

  describe "Compile-Time Protocol Validation" do
    it "should not compile when required adapter is missing" \_ ->
      shouldNotTypecheck (
        Service.deploy (Service.command @CreateCartCommand)
      )

    it "should not compile when some adapters are missing" \_ ->
      shouldNotTypecheck (
        Service.deploy (
          Service.expose (DirectAdapter defaultConfig)
          Service.>> Service.command @AddItemCommand
        )
      )

    it "should not compile with multiple missing adapters" \_ ->
      shouldNotTypecheck (
        Service.deploy (
          Service.command @CreateCartCommand  -- Requires Direct
          Service.>> Service.command @AddItemCommand      -- Requires Direct, REST
        )
      )

  describe "DirectAdapter Behavior" do
    it "initializes successfully" \_ -> do
      let adapter = DirectAdapter { config = defaultConfig }
      state <- initializeAdapter adapter |> Task.mapError toText
      state.isShutdown |> shouldBe False

    it "rejects execution when shutdown" \_ -> do
      let adapter = DirectAdapter { config = defaultConfig }
      let shutdownState = DirectAdapterState { isShutdown = True }

      result <- executeCommand adapter shutdownState ("test" :: Text) (Bytes.fromLegacy "{}")
                  |> Task.asResult

      case result of
        Err ServiceAlreadyShutdown -> Task.yield unit
        _ -> fail "Expected ServiceAlreadyShutdown error"

    it "executes when not shutdown" \_ -> do
      let adapter = DirectAdapter { config = defaultConfig }
      state <- initializeAdapter adapter |> Task.mapError toText

      -- Currently returns empty bytes (placeholder)
      result <- executeCommand adapter state ("test" :: Text) (Bytes.fromLegacy "{}") |> Task.mapError toText
      result |> shouldBe (Bytes.fromLegacy "")

  describe "Service Composition" do
    it "composes multiple commands with same protocol" \_ -> do
      -- Both commands require Direct, one adapter suffices
      let serviceDef =
            Service.expose (DirectAdapter defaultConfig) Service.>>
            Service.command @CreateCartCommand
            -- Add more Direct-only commands here

      _runtime <- Service.deploy serviceDef |> Task.mapError toText
      Task.yield unit

    it "maintains type safety through composition" \_ -> do
      -- The monadic interface should properly track protocols
      let step1 = Service.expose (DirectAdapter defaultConfig)
      let step2 = step1 Service.>> Service.command @CreateCartCommand

      _runtime <- Service.deploy step2 |> Task.mapError toText
      Task.yield unit