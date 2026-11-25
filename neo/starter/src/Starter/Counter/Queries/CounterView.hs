module Starter.Counter.Queries.CounterView (
  CounterView (..),
  canAccess,
  canView,
) where

import Core
import Derive
import Service.AccessControl (AccessError, UserClaims, publicAccess, publicView)
import Starter.Counter.Entity (CounterEntity (..))


data CounterView = CounterView
  { counterId :: Uuid
  , label :: Text
  , value :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = publicAccess


canView :: Maybe UserClaims -> CounterView -> Maybe AccessError
canView = publicView


projectionIdFromCounterEntity :: CounterEntity -> Uuid
projectionIdFromCounterEntity entity = entity.counterId


combineCounterEntity :: CounterEntity -> Maybe CounterView -> QueryAction CounterView
combineCounterEntity entity _ =
  Update CounterView {
    counterId = entity.counterId,
    label = entity.label,
    value = entity.value
  }


derive ''CounterView query { entities = [''CounterEntity] }
