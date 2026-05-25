{-# OPTIONS_GHC -Wno-unused-imports #-}

module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
) where

import Basics
import Service.QueryObjectStore.Core (QueryObjectStore (..), QueryObjectStoreConfig (..))
import Task (Task)
import Task qualified
import Text (Text)


data PostgresQueryObjectStoreConfig = PostgresQueryObjectStoreConfig
  { host :: Text,
    port :: Int,
    databaseName :: Text,
    user :: Text,
    password :: Text
  }


instance QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore _config = do
    Task.yield
      QueryObjectStore
        { get = panic "Postgres QueryObjectStore.get not yet implemented",
          atomicUpdate = panic "Postgres QueryObjectStore.atomicUpdate not yet implemented",
          getAll = panic "Postgres QueryObjectStore.getAll not yet implemented"
        }
