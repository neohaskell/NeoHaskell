module Test.Service.EntityFetcher.Core (
  CartEvent (..),
  CartState (..),
  initialState,
  applyEvent,
  newFetcher,
  newFetcherWithCache,
) where

import Array qualified
import Core
import Json qualified
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.EventStore.Core (EventStore)
import Service.SnapshotCache.Core (SnapshotCache)
import Test.Service.EventStore.Core (CartEvent (..))


-- | Example entity state for a shopping cart
data CartState = CartState
  { cartId :: Maybe Uuid,
    cartItems :: Array (Uuid, Int),
    isCheckedOut :: Bool,
    version :: Int
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CartState


instance Json.FromJSON CartState


-- | Initial state for a new cart
initialState :: CartState
initialState =
  CartState
    { cartId = Nothing,
      cartItems = Array.empty,
      isCheckedOut = False,
      version = 0
    }


-- | Apply a cart event to the current state
applyEvent :: CartEvent -> CartState -> CartState
applyEvent event state = do
  let newVersion = state.version + 1
  case event of
    CartCreated {entityId} ->
      CartState
        { cartId = Just entityId,
          cartItems = Array.empty,
          isCheckedOut = False,
          version = newVersion
        }
    ItemAdded {itemId, amount} -> do
      let existingItems = state.cartItems
      let updatedItems = addOrUpdateItem existingItems itemId amount
      state
        { cartItems = updatedItems,
          version = newVersion
        }
    ItemRemoved {itemId} -> do
      let updatedItems = Array.dropIf (\(id, _) -> id == itemId) state.cartItems
      state
        { cartItems = updatedItems,
          version = newVersion
        }
    CartCheckedOut {} ->
      state
        { isCheckedOut = True,
          version = newVersion
        }


addOrUpdateItem :: Array (Uuid, Int) -> Uuid -> Int -> Array (Uuid, Int)
addOrUpdateItem items itemId amount = do
  let existing = Array.find (\(id, _) -> id == itemId) items
  case existing of
    Just (_, currentAmount) -> do
      let filtered = Array.takeIf (\(id, _) -> id != itemId) items
      Array.append filtered (Array.wrap (itemId, currentAmount + amount))
    Nothing ->
      Array.append items (Array.wrap (itemId, amount))


-- | Create a new entity fetcher for carts
newFetcher :: EventStore CartEvent -> Task EntityFetcher.Error (EntityFetcher CartState CartEvent)
newFetcher eventStore = do
  EntityFetcher.new eventStore initialState applyEvent


-- | Create a new entity fetcher for carts with cache support
newFetcherWithCache ::
  EventStore CartEvent ->
  SnapshotCache CartState ->
  Task EntityFetcher.Error (EntityFetcher CartState CartEvent)
newFetcherWithCache eventStore cache = do
  EntityFetcher.newWithCache eventStore cache initialState applyEvent
