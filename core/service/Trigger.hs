module Trigger (
  Trigger (..),
  new,
) where

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
newtype Trigger (event :: Type) = Trigger ((event -> IO ()) -> IO ())


new ::
  forall (event :: Type).
  ((event -> IO ()) -> IO ()) ->
  Trigger event
new processConstructor = Trigger processConstructor
