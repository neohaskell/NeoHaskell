-- | Convenience re-export so callers can write `import Starter.Counter.Core`
-- instead of importing Entity and Event separately.
module Starter.Counter.Core (
  module Starter.Counter.Entity,
  module Starter.Counter.Event,
) where

import Starter.Counter.Entity
import Starter.Counter.Event
