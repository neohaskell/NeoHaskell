module Service.EventStore.Postgres.Internal.Notifications where

import Core
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Service.EventStore.Postgres.Internal.SubscriptionStore (SubscriptionStore)
import Task qualified


connectTo :: Sessions.Connection -> SubscriptionStore eventType -> Task Text Unit
connectTo _ _ = Task.yield (panic "connectTo - not implemented yet")