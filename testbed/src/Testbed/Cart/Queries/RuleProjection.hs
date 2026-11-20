-- | Project cart and stock histories into one row for rule runner examples.
-- The row uses a fixed ID so event and query IDs can be checked independently.
module Testbed.Cart.Queries.RuleProjection (RuleProjection (..), projectionId, canAccess, canView) where

import Array qualified
import Core
import Maybe qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.Cart.Core (CartEntity (..))
import Testbed.Stock.Core (StockEntity (..))
import Uuid qualified


data RuleProjection = RuleProjection
  { rowId :: Uuid,
    cartSeen :: Bool,
    stockSeen :: Bool,
    itemCount :: Int,
    stockCount :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.publicAccess claims


canView :: Maybe UserClaims -> RuleProjection -> Maybe AccessError
canView claims projection = AccessControl.publicView claims projection


deriveQuery ''RuleProjection [''CartEntity, ''StockEntity]


instance QueryOf CartEntity RuleProjection where
  queryId _ = projectionId


  combine cart previous = case cart.savedIBAN of
    Just "delete" -> Delete
    Just "noop" -> NoOp
    Just "same" -> case previous of
      Just current -> Update current
      Nothing -> NoOp
    _ -> do
      let current = previous |> Maybe.withDefault emptyProjection
      Update current {cartSeen = True, itemCount = Array.length cart.items}


instance QueryOf StockEntity RuleProjection where
  queryId _ = projectionId


  combine _ previous = do
    let current = previous |> Maybe.withDefault emptyProjection
    Update current {stockSeen = True, stockCount = current.stockCount + 1}


projectionId :: Uuid
projectionId = Uuid.generateV5 Uuid.nil "rule-projection-row"


emptyProjection :: RuleProjection
emptyProjection = RuleProjection {rowId = projectionId, cartSeen = False, stockSeen = False, itemCount = 0, stockCount = 0}
