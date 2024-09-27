module Neo.Triggers (
  triggers,
) where

import Array qualified
import Core
import Neo.Event (Event (..))


triggers :: Array (Trigger Event)
triggers = Array.empty