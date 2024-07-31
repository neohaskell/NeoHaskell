module Subscription
  ( Subscription,
    new,
    everyMilliseconds,
  )
where

import AsyncIO qualified
import Basics

-- |
-- A subscription is a way to listen to a process that's running in the background and
-- generates events. When you tell NeoHaskell to subscribe to a process, NeoHaskell will
-- pass a callback function that posts an event to the event queue whenever the process
-- generates an event. The callback function is called a `dispatch` function.
--
-- An example Subscription is `Time.every`, which generates an event every specified
-- milliseconds. When you subscribe to `Time.every`, you pass a callback function that
-- posts an event to the event queue whenever the process ticks.
newtype Subscription (message :: Type) = Subscription ((message -> IO ()) -> IO ())

new ::
  forall (message :: Type).
  ((message -> IO ()) -> IO ()) ->
  Subscription message
new processConstructor = Subscription processConstructor

-- | Create a subscription that generates an event every specified milliseconds.
everyMilliseconds ::
  forall (message :: Type).
  Int ->
  (Int -> message) ->
  Subscription message
everyMilliseconds milliseconds messageConstructor =
  -- TODO: Move to Time module
  new \dispatch -> forever do
    currentMs <- getCurrentTime
    dispatch (messageConstructor currentMs)
    AsyncIO.sleep (milliseconds * 1000)

-- FIXME: Implement this function in the Time module
getCurrentTime :: IO Int
getCurrentTime = dieWith "Not implemented"