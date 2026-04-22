-- | Inbound integration handle backed by a bounded 'Channel'.
--
-- @controllableHandle@ gives every 'InboundIntegration' fake a default
-- capacity of 1024 — matching the ADR-0055 §2 design — while
-- 'controllableHandleWith' lets an integration author pick a different bound.
module Service.Integration.Inbound
  ( InboundHandle (..),
    controllableHandle,
    controllableHandleWith,
    inject,
    drive,
    tryRead,
    defaultCapacity,
  )
where

import Basics
import Channel (Channel)
import Channel qualified
import Control.Concurrent.MVar qualified as MVar
import Control.Monad qualified as Monad
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Integration.IntegrationError (IntegrationError (..))
import System.IO.Unsafe qualified as Unsafe
import Task (Task)
import Task qualified
import Text (Text)


-- | A controllable inbound handle — tests push triggers via 'inject' and the
-- runtime consumes them via 'drive'. Backed by a lazily-initialised bounded
-- channel so the default 1024-capacity form can live at the typeclass
-- constraint level without demanding IO on every 'runFake' witness.
data InboundHandle trigger = InboundHandle
  { capacity :: Int,
    channelRef :: MVar.MVar (Maybe (Channel trigger)),
    -- | Error captured at construction time (e.g. invalid capacity).
    creationError :: Maybe Text
  }


defaultCapacity :: Int
defaultCapacity = 1024


-- | The default fake for an 'InboundIntegration' whose 'Trigger' has an
-- 'Arbitrary' instance.
controllableHandle :: forall trigger. InboundHandle trigger
controllableHandle = controllableHandleWith defaultCapacity
{-# NOINLINE controllableHandle #-}


-- | Controllable handle with an explicit capacity.
--
-- Uses 'unsafePerformIO' to allocate the 'MVar' slot — the NeoHaskell
-- convention is that lazy top-level references are allowed when no side
-- effect observable by user code occurs until the channel is actually used.
controllableHandleWith :: forall trigger. Int -> InboundHandle trigger
controllableHandleWith cap =
  Unsafe.unsafePerformIO do
    ref <- MVar.newMVar Nothing
    Monad.return
      InboundHandle
        { capacity = cap,
          channelRef = ref,
          creationError =
            if cap <= 0
              then Just "capacity must be positive"
              else Nothing
        }
{-# NOINLINE controllableHandleWith #-}


-- | Inject a synthetic trigger. Blocks when the backing channel is full —
-- this is ADR-0055's backpressure guarantee.
inject ::
  forall trigger.
  InboundHandle trigger ->
  trigger ->
  Task IntegrationError ()
inject handle trigger = do
  channel <- resolveChannel handle
  Channel.write trigger channel
    |> Task.mapError TransportFailure


-- | Drive the handle — pull triggers from the backing channel and feed each
-- to @callback@.
--
-- Returns 'Err TransientFailure' if the callback throws.
drive ::
  forall trigger.
  InboundHandle trigger ->
  (trigger -> Task IntegrationError ()) ->
  Task IntegrationError ()
drive handle callback = do
  channel <- resolveChannel handle
  loop channel
  where
    loop channel = do
      trigger <-
        Channel.read channel
          |> Task.mapError TransportFailure
      result <-
        callback trigger
          |> Task.asResult
      case result of
        Err err -> Task.throw err
        Ok () -> loop channel


-- | Poll the underlying channel without blocking.
tryRead ::
  forall trigger.
  InboundHandle trigger ->
  Task IntegrationError (Maybe trigger)
tryRead handle = do
  channel <- resolveChannel handle
  Channel.tryRead channel
    |> Task.mapError TransportFailure


-- | Resolve the backing channel, creating it on first use. Honours any
-- capacity validation error recorded at construction.
resolveChannel ::
  forall trigger.
  InboundHandle trigger ->
  Task IntegrationError (Channel trigger)
resolveChannel handle =
  case handle.creationError of
    Just err ->
      Task.throw (ValidationFailure err)
    Nothing -> do
      current <-
        MVar.readMVar handle.channelRef
          |> Task.fromIO
      case current of
        Just channel -> Task.yield channel
        Nothing -> do
          channel <-
            Channel.newBounded handle.capacity
              |> Task.mapError ValidationFailure
          Task.fromIO (MVar.modifyMVar_ handle.channelRef (\_ -> Monad.return (Just channel)))
          Task.yield channel
