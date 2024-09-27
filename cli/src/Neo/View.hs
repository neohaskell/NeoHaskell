module Neo.View (
  view,
) where

import Core
import Neo.Build qualified as Build
import Neo.State (State (..))


view :: State -> Text
view state =
  Build.view state.build
