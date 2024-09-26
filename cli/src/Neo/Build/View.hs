module Neo.Build.View (
  view,
) where

import Core
import Neo.Build.State (State (..))
import Text qualified


view :: State -> View
view state = do
  let configText = toText state.config
  let messagesText =
        state.messages
          |> Text.joinWith "\n\n"
  [fmt|
=============
Config:
  {configText}
=============


{messagesText}
  |]
