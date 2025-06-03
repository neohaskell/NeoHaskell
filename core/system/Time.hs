module Time (triggerEveryMilliseconds) where

import Basics
import Trigger (Trigger)


triggerEveryMilliseconds :: Int -> Trigger Int
triggerEveryMilliseconds _ =
  panic "Event handling is being reimplemented: Check on Discord for more information"


-- | Create a trigger that generates an event every specified milliseconds.
-- triggerEveryMilliseconds ::
--   forall (event :: Type).
--   Int ->
--   (Int -> event) ->
--   Trigger event
-- triggerEveryMilliseconds milliseconds messageConstructor =
--   Trigger.new \dispatch -> forever do
--     posixTime <- Nanotime.currentTime @Nanotime.PosixTime
--     let currentMs =
--           posixTime
--             |> Nanotime.unPosixTime
--             |> Prelude.fromIntegral
--     dispatch (messageConstructor currentMs)
--     AsyncTask.sleep milliseconds
