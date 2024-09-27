module Neo (
  State (..),
  Event (..),
  init,
  update,
  view,
  triggers,
) where

import Neo.Event (Event (..))
import Neo.State (State (..), init, update)
import Neo.Triggers (triggers)
import Neo.View (view)
