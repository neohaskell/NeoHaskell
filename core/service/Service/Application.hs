module Service.Application (
  -- * Application Type
  Application,

  -- * Construction
  new,

  -- * Configuration
  withQueryRegistry,

  -- * Inspection
  isEmpty,
  hasQueryRegistry,
) where

import Basics
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry


-- | Application combines multiple Services and Queries with shared infrastructure.
--
-- The Application type provides a builder pattern for configuring:
--
-- * QueryRegistry (maps entity names to query updaters)
--
-- Future additions will include:
--
-- * EventStore configuration (shared by all services)
-- * QueryObjectStore configuration (for storing query/read model data)
-- * Transport configuration (HTTP servers, CLI, etc.)
--
-- Example usage:
--
-- @
-- app = Application.new
--   |> Application.withQueryRegistry myRegistry
-- @
data Application = Application
  { queryRegistry :: QueryRegistry
  }


-- | Create a new empty Application.
new :: Application
new =
  Application
    { queryRegistry = Registry.empty
    }


-- | Add a QueryRegistry to the Application.
--
-- The QueryRegistry maps entity names to query updaters, which are called
-- when events are processed to update read models.
withQueryRegistry ::
  QueryRegistry ->
  Application ->
  Application
withQueryRegistry registry app =
  app {queryRegistry = registry}


-- | Check if the Application is empty (no configurations set).
isEmpty :: Application -> Bool
isEmpty app = Registry.isEmpty app.queryRegistry


-- | Check if a QueryRegistry has been configured.
hasQueryRegistry :: Application -> Bool
hasQueryRegistry app = not (Registry.isEmpty app.queryRegistry)
