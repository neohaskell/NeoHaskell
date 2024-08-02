module Trigger
  ( Trigger,
    new,
    everyMilliseconds,
  )
where

import AsyncIO qualified
import Basics

-- |
-- A trigger is a way to listen to a process that's running in the background and
-- generates events. When you tell NeoHaskell to subscribe to a process, NeoHaskell will
-- pass a callback function that posts an event to the event queue whenever the process
-- generates an event. The callback function is called a `dispatch` function.
--
-- An example Trigger is `Time.every`, which generates an event every specified
-- milliseconds. When you subscribe to `Time.every`, you pass a callback function that
-- posts an event to the event queue whenever the process ticks.
newtype Trigger (message :: Type) = Trigger ((message -> IO ()) -> IO ())

new ::
  forall (message :: Type).
  ((message -> IO ()) -> IO ()) ->
  Trigger message
new processConstructor = Trigger processConstructor

-- | Create a trigger that generates an event every specified milliseconds.
everyMilliseconds ::
  forall (message :: Type).
  Int ->
  (Int -> message) ->
  Trigger message
everyMilliseconds milliseconds messageConstructor =
  -- TODO: Move to Time module
  new \dispatch -> forever do
    currentMs <- getCurrentTime
    dispatch (messageConstructor currentMs)
    AsyncIO.sleep (milliseconds * 1000)

-- FIXME: Implement this function in the Time module
getCurrentTime :: IO Int
getCurrentTime = dieWith "Not implemented"