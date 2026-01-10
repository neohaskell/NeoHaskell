module Test.Service.QueryObjectStore.InMemory.Context (
  Context (..),
  initialize,
) where

import Core
import Service.QueryObjectStore.Core (QueryObjectStore)
import Test.Service.QueryObjectStore.Core (TestQuery)
import Uuid qualified


data Context = Context
  { store :: QueryObjectStore TestQuery,
    testId1 :: Uuid,
    testId2 :: Uuid,
    testId3 :: Uuid
  }


initialize :: Task Text (QueryObjectStore TestQuery) -> Task Text Context
initialize newStore = do
  store <- newStore
  testId1 <- Uuid.generate
  testId2 <- Uuid.generate
  testId3 <- Uuid.generate
  pure Context {store, testId1, testId2, testId3}
