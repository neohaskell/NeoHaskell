module Test.Service.QueryObjectStore.Core (
  TestQuery (..),
) where

import Basics
import Json qualified
import Text (Text)
import Uuid (Uuid)


-- | A simple test query type for testing QueryObjectStore.
data TestQuery = TestQuery
  { queryId :: Uuid,
    name :: Text,
    count :: Int
  }
  deriving (Eq, Show, Generic)


instance Json.FromJSON TestQuery


instance Json.ToJSON TestQuery
