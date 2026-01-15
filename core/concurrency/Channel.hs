module Channel (
  Channel,
  new,
  newBounded,
  read,
  write,
  tryWriteWithTimeout,
  isBounded,
) where

import Basics
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import TypeName qualified


-- | A channel for communication between concurrent tasks.
--
-- Channels can be either:
-- - **Unbounded**: Writers never block, but memory can grow without limit
-- - **Bounded**: Writers block when channel is full, providing backpressure
--
-- Use unbounded channels for low-volume or when backpressure isn't needed.
-- Use bounded channels for high-throughput scenarios to prevent OOM.
data Channel value
  = -- | Unbounded channel using unagi-chan (high performance, no backpressure)
    UnboundedChannel
      { outChannel :: Unagi.OutChan value,
        inChannel :: Unagi.InChan value
      }
  | -- | Bounded channel using STM TBQueue (backpressure when full)
    BoundedChannel
      { tbQueue :: TBQueue.TBQueue value,
        capacity :: Int
      }


instance (TypeName.Inspectable value) => Show (Channel value) where
  show channel = do
    let typeName = TypeName.reflect @value
    case channel of
      UnboundedChannel {} -> [fmt|[Channel (unbounded) #{typeName}]|]
      BoundedChannel {capacity} -> [fmt|[Channel (bounded: #{capacity}) #{typeName}]|]


instance (TypeName.Inspectable value) => Show (Unagi.OutChan value) where
  show _ = do
    let typeName = TypeName.reflect @value
    [fmt|[OutChan #{typeName}]|]


instance (TypeName.Inspectable value) => Show (Unagi.InChan value) where
  show _ = do
    let typeName = TypeName.reflect @value
    [fmt|[InChan #{typeName}]|]


-- | Create a new unbounded channel.
--
-- Writers never block, but memory can grow without limit if the consumer
-- is slower than producers. Use for low-volume scenarios.
new :: Task _ (Channel value)
new = do
  (inChannel, outChannel) <- Task.fromIO Unagi.newChan
  Task.yield (UnboundedChannel {outChannel, inChannel})


-- | Create a new bounded channel with the specified capacity.
--
-- Writers block when the channel is full, providing natural backpressure.
-- This prevents memory exhaustion when producers outpace consumers.
--
-- Capacity sizing rule of thumb:
-- @capacity = processing_rate * acceptable_latency@
--
-- For example: 10 events/second processing, 5 second acceptable latency = 50 capacity
--
-- Throws an error if capacity is not positive.
newBounded :: Int -> Task Text (Channel value)
newBounded channelCapacity = do
  if channelCapacity <= 0
    then Task.throw "Channel capacity must be positive"
    else do
      queue <- TBQueue.newTBQueueIO (fromIntegral channelCapacity) |> Task.fromIO
      Task.yield (BoundedChannel {tbQueue = queue, capacity = channelCapacity})


-- | Read a value from the channel.
--
-- Blocks until a value is available. Works the same for both bounded
-- and unbounded channels.
read :: Channel value -> Task _ value
read channel = case channel of
  UnboundedChannel {outChannel} ->
    Unagi.readChan outChannel
      |> Task.fromIO
  BoundedChannel {tbQueue} ->
    STM.atomically (TBQueue.readTBQueue tbQueue)
      |> Task.fromIO


-- | Write a value to the channel.
--
-- For unbounded channels: Never blocks, returns immediately.
-- For bounded channels: Blocks if the channel is full until space is available.
--
-- If you need timeout behavior for bounded channels, use 'tryWriteWithTimeout'.
write :: value -> Channel value -> Task _ Unit
write value channel = case channel of
  UnboundedChannel {inChannel} ->
    Unagi.writeChan inChannel value
      |> Task.fromIO
  BoundedChannel {tbQueue} ->
    STM.atomically (TBQueue.writeTBQueue tbQueue value)
      |> Task.fromIO


-- | Try to write a value to a bounded channel with a timeout.
--
-- Returns:
-- - @Ok ()@ if the write succeeded within the timeout
-- - @Err "timeout"@ if the channel was full for the entire timeout duration
--
-- For unbounded channels, this always succeeds immediately.
--
-- This is useful for the dispatcher to avoid blocking forever when a
-- worker's channel is full due to a slow integration.
--
-- Implementation uses STM registerDelay for atomic timeout behavior,
-- ensuring the result accurately reflects whether the write occurred.
tryWriteWithTimeout ::
  Int -> -- ^ Timeout in milliseconds
  value ->
  Channel value ->
  Task Text (Result Text Unit)
tryWriteWithTimeout timeoutMs value channel = case channel of
  UnboundedChannel {inChannel} -> do
    Unagi.writeChan inChannel value |> Task.fromIO
    Task.yield (Ok unit)
  BoundedChannel {tbQueue} -> do
    -- Use STM registerDelay for atomic timeout behavior.
    -- This ensures the write and timeout check happen in a single transaction,
    -- preventing the race condition where a write succeeds but timeout is reported.
    result <- Task.fromIO do
      timeoutVar <- STM.registerDelay (timeoutMs * 1000)
      STM.atomically do
        let writeAction = do
              TBQueue.writeTBQueue tbQueue value
              pure (Ok unit)
        let timeoutAction = do
              timedOut <- STM.readTVar timeoutVar
              if timedOut
                then pure (Err "timeout")
                else STM.retry
        writeAction `STM.orElse` timeoutAction
    Task.yield result


-- | Check if a channel is bounded.
isBounded :: Channel value -> Bool
isBounded channel = case channel of
  UnboundedChannel {} -> False
  BoundedChannel {} -> True
