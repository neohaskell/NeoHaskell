module Time (triggerEveryMilliseconds) where

import AsyncIO qualified
import Basics
import Nanotime qualified
import Trigger (Trigger)
import Trigger qualified
import Prelude qualified


-- | Create a trigger that generates an event every specified milliseconds.
triggerEveryMilliseconds ::
  forall (event :: Type).
  Int ->
  (Int -> event) ->
  Trigger event
triggerEveryMilliseconds milliseconds messageConstructor =
  Trigger.new \dispatch -> forever do
    posixTime <- Nanotime.currentTime @Nanotime.PosixTime
    let currentMs =
          posixTime
            |> Nanotime.unPosixTime
            |> Prelude.fromIntegral
    dispatch (messageConstructor currentMs)
    AsyncIO.sleep milliseconds
