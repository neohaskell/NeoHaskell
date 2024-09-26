-- | Index module that re-exports all the modules in the Neo.Build namespace.
module Neo.Build (
  Event (..),
  State (..),
  commandParser,
  initialState,
  update,
  view,
) where

import Neo.Build.Event (Event (..), commandParser)
import Neo.Build.State (State (..), initialState, update)
import Neo.Build.View (view)
