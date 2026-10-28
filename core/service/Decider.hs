module Decider (
  -- * The Decision Monad
  Decision (..),
  DecisionContext (..),
  CommandResult (..),

  -- * Execution
  runDecision,
  runDecisionWithKeys,

  -- * Smart Constructors
  generateUuid,
  protectValue,
  transformValue,
  generateDeterministicUuid,
  acceptAny,
  acceptNew,
  acceptExisting,
  acceptAfter,
  reject,
) where

import Applicable
import Array (Array)
import Array qualified
import Basics
import Erasable.Cipher qualified as Cipher
import Erasable.Internal (NewKey (..), ProtectedValue (..))
import Erasable.Internal qualified as Erasable
import EventVariantOf (EventVariantOf (..))
import Control.Monad qualified as Monad
import Log qualified
import Json qualified
import Result (Result (..))
import Mappable
import Service.Event (InsertionType (..), StreamPosition)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Maybe (Maybe (..))
import Thenable
import ToText (toText)
import Uuid (Uuid)
import Uuid qualified


-- | The result of running a Decision.
--
-- Either the command is accepted with events to persist, or rejected with a reason.
data CommandResult event
  = AcceptCommand InsertionType (Array event)
  | RejectCommand Text
  deriving (Eq, Show, Ord, Generic)


-- | The Decision monad for expressing command acceptance/rejection logic.
--
-- Decision is a free monad that allows you to:
--
-- * Generate UUIDs via 'generateUuid'
-- * Accept commands with events via 'acceptNew', 'acceptExisting', 'acceptAny', 'acceptAfter'
-- * Reject commands with a reason via 'reject'
--
-- Decisions are pure and can be easily tested without side effects.
data Decision a where
  Return :: a -> Decision a
  Bind :: Decision a -> (a -> Decision b) -> Decision b
  GenUuid :: Decision Uuid
  ProtectValue :: Json.Value -> Decision ProtectedValue
  TransformValue :: (Json.FromJSON input, Json.ToJSON output) => (input -> output) -> ProtectedValue -> Decision ProtectedValue
  Accept :: InsertionType -> Array a -> Decision a
  Reject :: Text -> Decision a


instance Functor Decision where
  fmap f m = m |> Thenable.andThen (\r -> f r |> Return)


instance Applicative Decision where
  pure = Return
  (<*>) = Monad.ap


instance Monad Decision where
  m >>= f = Bind m f


-- | Context required to execute a Decision.
--
-- Provides effectful capabilities like UUID generation.
data DecisionContext = DecisionContext
  { genUuid :: Task Text Uuid
  }


-- | Execute a Decision within a Task context, producing a CommandResult.
--
-- The Decision must terminate with either 'Accept' or 'Reject'. A bare 'Return'
-- or an 'Accept'/'Reject' in the middle of a chain will result in an error.
runDecision :: (HasCallStack) => DecisionContext -> Decision a -> Task Text (CommandResult a)
runDecision ctx decision = do
  (result, _) <- runDecisionWithKeys ctx (\_ -> Task.throw "Erasable key lookup is unavailable") decision
  Task.yield result


runDecisionWithKeys ::
  (HasCallStack) =>
  DecisionContext ->
  (Text -> Task Text (Maybe (Maybe Cipher.Key))) ->
  Decision a ->
  Task Text (CommandResult a, Array NewKey)
runDecisionWithKeys ctx lookupKey = go Array.empty
 where
  go :: forall a. (HasCallStack) => Array NewKey -> Decision a -> Task Text (CommandResult a, Array NewKey)
  go _ (Return _) = Task.throw "Decision didn't terminate with accept/reject"
  go keys (Bind m f) = case m of
    GenUuid -> do
      uuid <- ctx.genUuid
      f uuid |> go keys
    ProtectValue jsonValue -> do
      uuid <- ctx.genUuid
      key <- Cipher.generateKey
      let reference = Uuid.toText uuid
      let plaintext = jsonValue |> Json.encodeText |> Text.toBytes
      let context = "decision"
      envelope <- Cipher.seal key (Erasable.contextBytes reference context) plaintext
      let protected = ProtectedValue {reference, context, envelope}
      let newKey = NewKey {reference, key}
      f protected |> go (keys |> Array.push newKey)
    TransformValue transform protected -> do
      keyState <-
        case Array.find (\entry -> entry.reference == protected.reference) keys of
          Just entry -> Task.yield (Just (Just entry.key))
          Nothing -> lookupKey protected.reference
      case keyState of
        Nothing -> Task.throw "Erasable key record is missing"
        Just Nothing -> f protected |> go keys
        Just (Just key) -> do
          bytes <-
            case Cipher.open key (Erasable.contextBytes protected.reference protected.context) protected.envelope of
              Err err -> Task.throw err
              Ok value -> Task.yield value
          source <-
            case Json.decodeBytes bytes of
              Err _ -> Task.throw "Erasable source is not valid JSON"
              Ok value -> Task.yield value
          let transformed = transform source |> Json.toJSON |> Json.encodeText |> Text.toBytes
          envelope <- Cipher.seal key (Erasable.contextBytes protected.reference protected.context) transformed
          f (protected {envelope}) |> go keys
    Accept _ _ -> Task.throw "Accept must be the last statement"
    Reject _ -> Task.throw "Reject must be the last statement"
    Return a -> go keys (f a)
    Bind m' f' -> go keys (Bind m' (\x -> Bind (f' x) f))
  go _ GenUuid = Task.throw "Unbound GenUuid"
  go _ (ProtectValue _) = Task.throw "Unbound ProtectValue"
  go _ (TransformValue _ _) = Task.throw "Unbound TransformValue"
  go keys (Accept s events) = do
    let eventCount = Array.length events
    Log.debug [fmt|Decision: accepted with #{toText eventCount} event(s)|] |> Task.ignoreError
    (AcceptCommand s events, keys) |> Task.yield
  go _ (Reject reason) = do
    Log.debug "Decision: rejected" |> Task.ignoreError
    (RejectCommand reason, Array.empty) |> Task.yield


protectValue :: Json.Value -> Decision ProtectedValue
protectValue value = ProtectValue value


transformValue :: (Json.FromJSON input, Json.ToJSON output) => (input -> output) -> ProtectedValue -> Decision ProtectedValue
transformValue transform protected = TransformValue transform protected


-- | Generate a UUID within a Decision context.
--
-- This allows you to generate unique identifiers when constructing events.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   cartId <- Decision.generateUuid
--   Decision.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
generateUuid :: Decision Uuid
generateUuid = GenUuid


-- | Derive a deterministic UUID (version 5, RFC 4122) within a Decision.
--
-- The same namespace and name always produce the same UUID, so it is the
-- counterpart to 'generateUuid': /random for fresh identity, deterministic for
-- derived identity/. It draws nothing from the decision context — the value is
-- a pure function of its inputs.
--
-- Example — deriving a stable id from a natural key:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Just _ ->
--       Decider.reject "Already registered!"
--     Nothing -> do
--       ownerId <- Decider.generateDeterministicUuid ownerNamespace cmd.externalUserId
--       Decider.acceptNew [ProjectRegistered {ownerId = ownerId}]
-- @
--
-- To derive the /entity's own/ id from a natural key, use 'Uuid.generateV5'
-- directly in the command's @getEntityId@ — that is what makes the same key
-- resolve to the same stream, so a repeat submission can be rejected here.
--
-- SECURITY: the result is __not a secret__ — see 'Uuid.generateV5'.
generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
generateDeterministicUuid namespace name =
  Uuid.generateV5 namespace name |> Return


-- | Accept a command regardless of stream state, emitting the given events.
--
-- Use this when you don't care whether the entity exists or not. This is the least
-- safe option as it doesn't enforce any stream state constraints.
--
-- Events can be the entity's event ADT directly, or any type with an
-- 'EventVariantOf' instance for that ADT.
--
-- Example:
--
-- @
-- decide cmd entity =
--   Decider.acceptAny [EventOccurred]
-- @
acceptAny :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptAny variants = do
  let events = variants |> Array.map fromVariant
  Accept AnyStreamState events


-- | Accept a command only if the stream doesn't exist yet, emitting the given events.
--
-- Use this for entity creation commands. The command will fail if the entity already exists,
-- preventing duplicate creation.
--
-- Events can be the entity's event ADT directly, or any type with an
-- 'EventVariantOf' instance for that ADT.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Just _ ->
--       Decider.reject "Cart already exists!"
--     Nothing -> do
--       cartId <- Decider.generateUuid
--       Decider.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
acceptNew :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptNew variants = do
  let events = variants |> Array.map fromVariant
  Accept StreamCreation events


-- | Accept a command only if the stream already exists, emitting the given events.
--
-- Use this for entity update commands. The command will fail if the entity doesn't exist,
-- ensuring you can't update something that hasn't been created.
--
-- Events can be the entity's event ADT directly, or any type with an
-- 'EventVariantOf' instance for that ADT.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Nothing ->
--       Decider.reject "Cart does not exist!"
--     Just _ ->
--       Decider.acceptExisting [CartUpdated]
-- @
acceptExisting :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptExisting variants = do
  let events = variants |> Array.map fromVariant
  Accept ExistingStream events


-- | Accept a command only if the stream is at the specified position, emitting the given events.
--
-- Use this for optimistic concurrency control. The command will fail if the stream has
-- been modified since the position was read, preventing concurrent modification conflicts.
--
-- Events can be the entity's event ADT directly, or any type with an
-- 'EventVariantOf' instance for that ADT.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Nothing ->
--       Decider.reject "Cart does not exist!"
--     Just e -> do
--       let expectedPosition = e.version
--       Decider.acceptAfter expectedPosition [CartUpdated]
-- @
acceptAfter :: forall event variant. (EventVariantOf event variant) => StreamPosition -> Array variant -> Decision event
acceptAfter pos variants = do
  let events = variants |> Array.map fromVariant
  Accept (InsertAfter pos) events


-- | Reject a command with a reason message.
--
-- Use this when a command cannot be processed due to business rule violations,
-- invalid state, or other constraints.
--
-- Example:
--
-- @
-- decide cmd entity = do
--   case entity of
--     Just _ ->
--       Decision.reject "Cart already exists!"
--     Nothing -> do
--       cartId <- Decision.generateUuid
--       Decision.acceptNew [CartCreated {cartCreatedId = cartId}]
-- @
reject :: Text -> Decision a
reject reason = Reject reason
