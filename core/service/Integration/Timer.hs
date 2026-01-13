-- | # Integration.Timer
--
-- Timer-based inbound integrations for periodic command submission.
--
-- == Usage
--
-- @
-- import Integration qualified
-- import Integration.Timer qualified as Timer
--
-- periodicCartCreator :: Integration.Inbound
-- periodicCartCreator =
--   Timer.every
--     { interval = Timer.seconds 30
--     , toCommand = \_ -> CreateCart
--     }
-- @
module Integration.Timer (
  -- * Timer Config
  Every (..),
  every,

  -- * Interval Helpers
  Interval,
  seconds,
  minutes,
  hours,
) where

import AsyncTask qualified
import Basics
import Integration qualified
import Json qualified
import TypeName qualified


-- | Time interval for periodic integrations (in milliseconds).
type Interval = Int


-- | Create an interval from seconds.
--
-- @
-- Timer.seconds 30  -- 30 seconds
-- @
seconds :: Int -> Interval
seconds n = n * 1000


-- | Create an interval from minutes.
--
-- @
-- Timer.minutes 5  -- 5 minutes
-- @
minutes :: Int -> Interval
minutes n = n * 60 * 1000


-- | Create an interval from hours.
--
-- @
-- Timer.hours 1  -- 1 hour
-- @
hours :: Int -> Interval
hours n = n * 3600 * 1000


-- | Config record for periodic command submission.
--
-- @
-- Timer.every
--   { interval = Timer.seconds 30
--   , toCommand = \tick -> CreateCart
--   }
-- @
data Every command = Every
  { interval :: Interval
  , toCommand :: Int -> command  -- ^ Tick count passed to command generator
  }


-- | Create an inbound integration that fires periodically.
--
-- The 'toCommand' function receives a tick count (starting at 1)
-- each time the timer fires.
--
-- @
-- periodicCartCreator :: Integration.Inbound
-- periodicCartCreator =
--   Timer.every
--     { interval = Timer.seconds 30
--     , toCommand = \_ -> CreateCart
--     }
-- @
every ::
  forall command.
  (Json.ToJSON command, TypeName.Inspectable command) =>
  Every command ->
  Integration.Inbound
every config = do
  let loop emit tick = do
        AsyncTask.sleep config.interval
        emit (config.toCommand tick)
        loop emit (tick + 1)
  Integration.inbound
    Integration.InboundConfig
      { run = \emit -> loop emit 1
      }
