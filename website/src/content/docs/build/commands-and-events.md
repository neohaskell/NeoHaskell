---
title: "Commands and events"
description: Add a business action while keeping requests, accepted facts, and state separate.
sidebar:
  order: 2
---

An application must distinguish what someone requested from what it accepted. That distinction gives you a place to express rules, explain refusals, and question an agent's implementation.

A **command** names an intention. An **event** names an accepted fact. In your `mug-shop` project, `AddItem` requests a selection and quantity; `ItemAdded` records an addition the Cart accepted. The [event model](/start/event-modeling/) gives those names a shared meaning.

This page continues the first working Cart from [your first working slice](/build/first-cart/). The first page created every source file needed to run that slice. Here we add one action by creating or replacing specific files in that same project. The focused snippets explain the decisions first; the assembled files later contain the real module headers and imports.

## Choose the rule before the files

From the `mug-shop` project root, stop `neo run` while you edit. The directories under `src/Shop/Cart/Commands`, `src/Shop/Cart/Events`, and `src/Shop/Cart/Queries` already exist from the first slice. If you are entering this page directly, create them with:

```sh
mkdir -p src/Shop/Cart/Commands src/Shop/Cart/Events src/Shop/Cart/Queries
```

We will require an existing Cart and a positive quantity. Each accepted addition becomes one entry, even when the same stock is selected again. Availability and ownership are separate policies covered in [stock](/build/stock-and-checkout/) and [access control](/build/access-control/). This action records a selection; it does not claim that stock was reserved.

## 1. Give the new fact its own home

Create `src/Shop/Cart/Events/ItemAdded.hs`. Its payload preserves the identifiers and quantity needed to explain the accepted addition:

```haskell
data Event = Event
  { entityId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
```

`entityId` keeps the fact on the Cart stream. `stockId` identifies the selected stock record, and `quantity` records the input that passed the command's rule. Derive the payload's standard event support with the canonical helper:

```haskell
deriveEvent ''Event
```

This is a new file, so its full contents appear in the assembled checkpoint below.

## 2. Expand the Cart event vocabulary

The first slice's `src/Shop/Cart/Event.hs` already defines `CartEvent` with `CartCreated`. Replace that event declaration with the expanded list:

```haskell
data CartEvent
  = CartCreated CartCreated.Event
  | ItemAdded ItemAdded.Event
```

Edit the existing `getEventEntityId` function in the same file by adding the new case:

```haskell
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId
  ItemAdded fact -> fact.entityId
```

Keep `deriveEvent ''CartEvent` after these declarations. `ItemAdded.Event` is the payload; `ItemAdded` is its constructor in the domain's list of accepted facts. The marker supplies routine event support, while the names and fields remain your business model.

## 3. Retain the selection in Cart state

Create `src/Shop/Cart/Item.hs` for the value stored in each Cart entry:

```haskell
data CartItem = CartItem {stockId :: Uuid, quantity :: Int}
```

The complete value type also supplies the JSON instances it needs. Now replace `src/Shop/Cart/Entity.hs` with the version that adds an `items` array. Its new update branch appends one entry:

```haskell
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}
```

The update function applies an accepted fact; it does not validate a request or contact a supplier. The command below admits only positive quantities. Any other producer of `ItemAdded` must preserve that invariant, because replay treats the event as an accepted fact.

The existing `CartCreated` branch must also initialize `items` to `Array.empty`. Keep that initialization when you replace the file.

## 4. Implement the decision

Create `src/Shop/Cart/Commands/AddItem.hs`. The request tells the command executor which Cart stream to load:

```haskell
getEntityId :: AddItem -> Maybe Uuid
getEntityId request = Just request.cartId
```

The decision refuses a missing Cart, then checks the quantity. Notice that the event retains the accepted input:

```haskell
decide request existing _context = case existing of
  Nothing -> Decider.reject "Cart not found!"
  Just cart -> addToCart request cart

addToCart request cart =
  if request.quantity <= 0
    then Decider.reject "Quantity must be positive"
    else Decider.acceptExisting
      [ItemAdded (ItemAdded.Event {entityId = cart.cartId, stockId = request.stockId, quantity = request.quantity})]
```

The command's `cartId` becomes the event's `entityId`; `stockId` is the selected stock identifier, not a product name. Its transport declaration exposes the request over the web transport. The command marker generates the routine plumbing from the decision, entity, and transport declarations above it:

```haskell
type instance EntityOf AddItem = CartEntity
type instance TransportsOf AddItem = '[WebTransport]

deriveCommand ''AddItem
```

## 5. Register the action and refresh the answer

Replace `src/Shop/Cart/Service.hs` with a registry containing both commands. The new line belongs beside the existing `CreateCart` registration:

```haskell
service = Service.new
  |> Service.command @CreateCart
  |> Service.command @AddItem
```

Replace `src/Shop/Cart/Queries/CartSummary.hs` so its projection counts the current entries:

```haskell
  combine cart _previous = do
    let count = cart.items |> Array.length
    Update CartSummary
      { cartSummaryId = cart.cartId
      , ownerId = cart.ownerId
      , itemCount = count
      , isEmpty = count == 0
      }
```

Keep `src/Shop/Cart/Core.hs` and `src/App.hs` from the first slice. The application already registers the Cart service and query; changing the service and projection makes the new command reachable and visible. The [queries lesson](/build/queries/) explains this read model and its asynchronous update in more depth.

## Create the complete Cart-additions files

The following blocks are the assembled files for this checkpoint. Each title is the exact path relative to the `mug-shop` project root. Create the new files and replace the files called out above.

### `src/Shop/Cart/Events/ItemAdded.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Events/ItemAdded.hs"
module Shop.Cart.Events.ItemAdded (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Eq)

deriveEvent ''Event
```

### `src/Shop/Cart/Event.hs` — replace

<!-- complete-file -->
```haskell title="src/Shop/Cart/Event.hs"
module Shop.Cart.Event (CartEvent (..), getEventEntityId) where

import Core
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Shop.Cart.Events.ItemAdded qualified as ItemAdded

data CartEvent
  = CartCreated CartCreated.Event
  | ItemAdded ItemAdded.Event
  deriving (Eq)

getEventEntityId :: CartEvent -> Uuid
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId
  ItemAdded fact -> fact.entityId

deriveEvent ''CartEvent
```

### `src/Shop/Cart/Item.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Item.hs"
module Shop.Cart.Item (CartItem (..)) where

import Core
import Json qualified

data CartItem = CartItem {stockId :: Uuid, quantity :: Int}
  deriving (Generic)

instance Json.FromJSON CartItem
instance Json.ToJSON CartItem
```

### `src/Shop/Cart/Entity.hs` — replace

<!-- complete-file -->
```haskell title="src/Shop/Cart/Entity.hs"
module Shop.Cart.Entity (CartEntity (..), initialState, update) where

import Core
import Shop.Cart.Event (CartEvent (..), getEventEntityId)
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Uuid qualified
import Array qualified
import Shop.Cart.Item (CartItem (..))
import Shop.Cart.Events.ItemAdded qualified as ItemAdded

data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  , items :: Array CartItem
  }

initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = "", items = Array.empty}

update :: CartEvent -> CartEntity -> CartEntity
update change cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId, items = Array.empty}
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}

deriveEntity ''CartEntity ''CartEvent
```

### `src/Shop/Cart/Commands/AddItem.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Commands/AddItem.hs"
module Shop.Cart.Commands.AddItem (AddItem (..), getEntityId, decide) where

import Core
import Shop.Cart.Events.ItemAdded qualified as ItemAdded
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.Transport.Web (WebTransport)
import Shop.Cart.Core (CartEntity (..), CartEvent (..))

data AddItem = AddItem {cartId :: Uuid, stockId :: Uuid, quantity :: Int}

getEntityId :: AddItem -> Maybe Uuid
getEntityId request = Just request.cartId

decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide request existing _context = case existing of
  Nothing -> Decider.reject "Cart not found!"
  Just cart -> addToCart request cart

addToCart :: AddItem -> CartEntity -> Decision CartEvent
addToCart request cart =
  if request.quantity <= 0
    then Decider.reject "Quantity must be positive"
    else Decider.acceptExisting
      [ItemAdded (ItemAdded.Event {entityId = cart.cartId, stockId = request.stockId, quantity = request.quantity})]

type instance EntityOf AddItem = CartEntity
type instance TransportsOf AddItem = '[WebTransport]

deriveCommand ''AddItem
```

### `src/Shop/Cart/Queries/CartSummary.hs` — replace

<!-- complete-file -->
```haskell title="src/Shop/Cart/Queries/CartSummary.hs"
module Shop.Cart.Queries.CartSummary (CartSummary (..), canAccess, canView) where

import Array qualified
import Core
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Shop.Cart.Core (CartEntity (..))

data CartSummary = CartSummary
  { cartSummaryId :: Uuid
  , ownerId :: Text
  , itemCount :: Int
  , isEmpty :: Bool
  }

canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.publicAccess

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.publicView

deriveQuery ''CartSummary [''CartEntity]

instance QueryOf CartEntity CartSummary where
  queryId cart = cart.cartId
  combine cart _previous = do
    let count = cart.items |> Array.length
    Update CartSummary
      { cartSummaryId = cart.cartId
      , ownerId = cart.ownerId
      , itemCount = count
      , isEmpty = count == 0
      }
```

### `src/Shop/Cart/Service.hs` — replace

<!-- complete-file -->
```haskell title="src/Shop/Cart/Service.hs"
module Shop.Cart.Service (service) where

import Core
import Service qualified
import Shop.Cart.Commands.AddItem (AddItem)
import Shop.Cart.Commands.CreateCart (CreateCart)

service :: Service _ _
service = Service.new
  |> Service.command @CreateCart
  |> Service.command @AddItem
```

The first-slice `src/App.hs`, `src/Shop/Cart/Core.hs`, `CreateCart.hs`, and `Events/CartCreated.hs` remain in place. The [cart additions archive](/examples/mug-shop-cart.tar.gz) is a convenient comparison checkpoint; this page contains the files needed for the implemented addition.

## Check the new behaviour

From the `mug-shop` project root:

```sh
neo build
neo run
```

Create a new Cart with the request in [your first working slice](/build/first-cart/), then replace `YOUR-CART-UUID` below. The fixed stock UUID is an illustrative selection until the Stock lesson creates its real record.

```sh
curl -i http://localhost:8080/commands/add-item \
  -H 'Content-Type: application/json' \
  --data '{"cartId":"YOUR-CART-UUID","stockId":"11111111-1111-1111-1111-111111111111","quantity":2}'
```

Expect acceptance, then a summary with one entry and `isEmpty: false`. One entry contains two units. Send quantity zero: expect HTTP 400 with `reason: "Quantity must be positive"`, while the accepted count remains one.

The transport declaration, service registration, and application registration together expose `/commands/add-item`. A type sitting in a file is not yet a reachable feature. The read model may take a short time to catch up; query again rather than submitting the addition twice.

## Exercise: a per-Cart limit

Choose a limit of six mugs **per Cart**. Your agent rejects requests above six and says the work is complete. What case has it missed?

<details>
<summary>Suggested reasoning and evidence</summary>

Two additions of four each pass that check but total eight. Specify whether the limit covers one product or every product, then compare existing quantities plus the request. Check a normal addition, exactly six, more than six, and another addition after reaching six. A refused operation must not produce a successful `ItemAdded`. This is an extension you design, not a rule already in these files.

</details>

Next: [entities and state](/build/entities-and-state/) explains how accepted facts inform the next decision.
