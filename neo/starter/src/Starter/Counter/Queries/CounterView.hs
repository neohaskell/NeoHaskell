{-# LANGUAGE TemplateHaskell #-}

module Starter.Counter.Queries.CounterView (
  CounterView (..),
  canAccess,
  canView,
) where

import Core
import Service.AccessControl (AccessError, UserClaims, publicAccess, publicView)
import Starter.Counter.Core (CounterEntity (..))


-- | Projected read model for a counter: one row per counter, updated every
-- time the underlying `CounterEntity` changes.
data CounterView = CounterView
  { counterId :: Uuid
  , label :: Text
  , value :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = publicAccess


canView :: Maybe UserClaims -> CounterView -> Maybe AccessError
canView = publicView


deriveQuery ''CounterView [''CounterEntity]


instance QueryOf CounterEntity CounterView where
  queryId entity = entity.counterId

  combine entity _maybeExisting =
    Update
      CounterView
        { counterId = entity.counterId
        , label = entity.label
        , value = entity.value
        }
