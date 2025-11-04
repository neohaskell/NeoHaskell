module Service.EventStore.Postgres (
  Config (..),
  new,
) where

import Core
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal (Config (..))
import Service.EventStore.Postgres.Internal qualified as Internal


new :: Config -> Task Text EventStore
new config = Internal.new (Internal.defaultOps config) config