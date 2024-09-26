module Neo.Core.Event (
  Event (..),
) where

import Core
import Neo.Build.Event qualified as Build


data Event
  = Build Build.Event
  | NoOp
  deriving (Show, Eq, Ord)
