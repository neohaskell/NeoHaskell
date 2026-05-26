module Service.QueryObjectStore.PostgresSpec where

import Core
import Service.QueryObjectStore.Core (QueryObjectStoreConfig (..))
import Service.QueryObjectStore.Postgres (PostgresQueryObjectStoreConfig (..))
import Test
import Test.Service.QueryObjectStore qualified as QueryObjectStore


spec :: Spec Unit
spec = do
  describe "Service.QueryObjectStore.Postgres" do
    let config =
          PostgresQueryObjectStoreConfig
            { host = "localhost"
            , port = 5432
            , databaseName = "neohaskell"
            , user = "neohaskell"
            , password = "neohaskell"
            }
    let newStore = createQueryObjectStore config
    QueryObjectStore.spec newStore
