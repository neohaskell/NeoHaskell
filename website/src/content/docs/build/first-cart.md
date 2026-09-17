---
title: "Your first working slice"
description: Give your own project one request, one recorded fact, and a useful answer.
sidebar:
  order: 1
---

The smallest useful application slice connects a person's request to something they can observe. Here you will build that slice in **your own `mug-shop` project**: accept “create a cart,” remember that it happened, and show an empty-cart summary.

The cart is our practice example. The same shape can start a booking or a document review. You decide what the action means; NeoHaskell connects the request, history, state, and view.

We will assemble the slice one responsibility at a time. Each section explains the idea before showing a focused piece. Then, after all of the decisions are clear, the page gives you every application source file at its real destination. You can create the project by hand without downloading an archive or guessing which definitions and imports are missing.

## Start in your own project

Complete [getting started](/getting-started/) first. That page already created `mug-shop` with `neo new mug-shop`. Open a terminal in that existing project:

```sh
cd mug-shop
```

Every path on this page is relative to that `mug-shop` directory. Keep its `neo.json`, launcher, and generated build setup. `neo` supplies the project's compiler configuration; application files do not need language pragmas.

The generated project contains a Counter example. Remove or move those supplied application files before creating the Cart files:

```sh
rm -r src/Starter tests/Decider/Counter
rm tests/Property/CounterReplaySpec.hs
rm tests/scenarios/counter-flow.hurl tests/integration/smoke.hurl
mkdir -p src/Shop/Cart/Commands src/Shop/Cart/Events src/Shop/Cart/Queries
```

Keep `tests/Spec.hs`; the [testing lesson](/build/testing/) adds the Cart test files. The source files below are the complete first slice. They replace `src/App.hs` and create the files under `src/Shop/Cart/`. `neo build` discovers those source files; you do not maintain a separate module list.

The framework module named `Core` and the small domain facade named `Shop.Cart.Core` have different jobs. Files that use framework types import `Core`. `Shop.Cart.Core` re-exports the Cart entity and event types so Cart commands and queries can share one domain-facing import.

## 1. Name the fact you want to remember

Start with the accepted fact, because it is the durable answer to “what happened?” The fact is **a cart was created**. It needs the cart's identifier and an owner identifier. Create `src/Shop/Cart/Events/CartCreated.hs` and begin with this focused declaration:

```haskell
data Event = Event
  { entityId :: Uuid
  , ownerId :: Text
  }
```

The fields are the information that gives the fact meaning when it is read later. The marker tells NeoHaskell to provide the routine event support:

```haskell
deriveEvent ''Event
```

The declaration says what the event means; the marker supplies mechanical instances and event plumbing. The complete file appears below after we have named the event's place in the Cart model.

## 2. Give the Cart event a home and a route

Create `src/Shop/Cart/Event.hs`. The domain event type lists the facts that can change a cart. At this first milestone it has one constructor:

```haskell
data CartEvent
  = CartCreated CartCreated.Event
```

`CartCreated.Event` is the payload from the file above. `CartCreated` is the constructor in the Cart's event vocabulary. The routing helper returns the stream identifier for the fact:

```haskell
getEventEntityId :: CartEvent -> Uuid
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId
```

Keep `getEventEntityId` in the event module. The entity file imports it before its `deriveEntity` marker, so replay can associate each fact with the Cart it changes. The `deriveEvent` marker belongs after these declarations.

## 3. Turn the fact into current state

An entity is the current business state reconstructed from its accepted events. Create `src/Shop/Cart/Entity.hs`. For the first slice, a Cart only needs an identifier and owner:

```haskell
data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  }
```

Reconstruction begins with a nil identifier and empty owner, then applies the creation fact:

```haskell
initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = ""}

update :: CartEvent -> CartEntity -> CartEntity
update change _cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId}
```

The initial nil value is a starting point for replay. It is not evidence that a real Cart exists; an accepted `CartCreated` establishes that identity. Put `initialState` and `update` before `deriveEntity ''CartEntity ''CartEvent`. You supply this business behaviour; `deriveEntity` connects it to the framework's replay, JSON, default-state, and event-routing support.

## 4. Accept the person's request

`CreateCart` is a command: a request someone makes. It has no input fields because this application generates the Cart identity. Create `src/Shop/Cart/Commands/CreateCart.hs`.

The decision first refuses a stream that already has state, then delegates creation:

```haskell
decide :: CreateCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ existing context = case existing of
  Just _ -> Decider.reject "Cart already exists!"
  Nothing -> createCart context
```

The helper generates a Cart UUID and records `CartCreated`. When no signed-in identity exists, this local exercise generates an anonymous owner identifier. That label in history does not establish a browser session or prove that a future caller owns the Cart; [access control](/build/access-control/) makes that policy explicit later.

The command also declares which entity and transport it uses. Its marker comes from the framework-facing `Core` import:

```haskell
type instance EntityOf CreateCart = CartEntity
type instance TransportsOf CreateCart = '[WebTransport]

deriveCommand ''CreateCart
```

The complete command file includes the UUID generation and both decision branches.

## 5. Answer the screen's question

A screen needs a useful answer, not the whole event history. Define a `CartSummary` in `src/Shop/Cart/Queries/CartSummary.hs` with the question the first screen asks:

```haskell
data CartSummary = CartSummary
  { cartSummaryId :: Uuid
  , ownerId :: Text
  , itemCount :: Int
  , isEmpty :: Bool
  }
```

At this milestone every Cart is empty, so the query's first projection intentionally sets `count` to zero:

```haskell
    let count = 0
    Update CartSummary
      { cartSummaryId = cart.cartId
      , ownerId = cart.ownerId
      , itemCount = count
      , isEmpty = count == 0
      }
```

This is a read model. It does not decide whether a Cart can be created. Its public access policy is deliberate for this local practice; private application data needs a different policy and tests. The query marker connects the view to the entity it reads:

```haskell
deriveQuery ''CartSummary [''CartEntity]
```

The complete query file keeps the marker before its `QueryOf` instance, because the instance uses the `Query` support that the marker generates.

## 6. Make the pieces reachable

The service is the Cart command registry. Create `src/Shop/Cart/Service.hs` and register `CreateCart`:

```haskell
service :: Service _ _
service = Service.new
  |> Service.command @CreateCart
```

Replace the generated `src/App.hs` so the application selects its event store, web transport, Cart service, and Cart query:

```haskell
app :: Application
app = Application.new
  |> Application.withEventStore @() (\_ -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = False
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
```

The complete `App.hs` below supplies the `eventStore` configuration. It uses `persistent = False`, so restarting clears this exercise's history. [Configuration](/build/configuration/) and [persistence](/operate/persistence/) later make storage an explicit choice.

## Create the complete first-slice files

The following blocks are assembled files, not teaching fragments. Each title is the path to create or replace from the `mug-shop` project root. Copy each block as written.

The complete event files include `deriving (Eq)` because the decider examples compare recorded payload values. That equality support is separate from the event marker; `deriveEvent` remains the canonical helper for the framework-generated event instances.

### `src/App.hs` — replace the generated application

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart

app :: Application
app = Application.new
  |> Application.withEventStore @() (\_ -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = False
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
```

### `src/Shop/Cart/Events/CartCreated.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Events/CartCreated.hs"
module Shop.Cart.Events.CartCreated (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , ownerId :: Text
  }
  deriving (Eq)

deriveEvent ''Event
```

### `src/Shop/Cart/Event.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Event.hs"
module Shop.Cart.Event (CartEvent (..), getEventEntityId) where

import Core
import Shop.Cart.Events.CartCreated qualified as CartCreated

data CartEvent
  = CartCreated CartCreated.Event
  deriving (Eq)

getEventEntityId :: CartEvent -> Uuid
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId

deriveEvent ''CartEvent
```

### `src/Shop/Cart/Entity.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Entity.hs"
module Shop.Cart.Entity (CartEntity (..), initialState, update) where

import Core
import Shop.Cart.Event (CartEvent (..), getEventEntityId)
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Uuid qualified

data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  }

initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = ""}

update :: CartEvent -> CartEntity -> CartEntity
update change _cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId}

deriveEntity ''CartEntity ''CartEvent
```

### `src/Shop/Cart/Core.hs` — create the domain facade

<!-- complete-file -->
```haskell title="src/Shop/Cart/Core.hs"
module Shop.Cart.Core (
  module Shop.Cart.Entity,
  module Shop.Cart.Event,
) where

import Shop.Cart.Entity
import Shop.Cart.Event
```

### `src/Shop/Cart/Commands/CreateCart.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Commands/CreateCart.hs"
module Shop.Cart.Commands.CreateCart (CreateCart (..), getEntityId, decide) where

import Core
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Decider qualified
import Service.Auth (RequestContext (..), UserClaims (..))
import Service.Command.Core (TransportsOf)
import Service.Transport.Web (WebTransport)
import Shop.Cart.Core (CartEntity (..), CartEvent (..))
import Uuid qualified

data CreateCart = CreateCart

getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing

decide :: CreateCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ existing context = case existing of
  Just _ -> Decider.reject "Cart already exists!"
  Nothing -> createCart context

createCart :: RequestContext -> Decision CartEvent
createCart context = do
  cartId <- Decider.generateUuid
  case context.user of
    Just user ->
      Decider.acceptNew [CartCreated (CartCreated.Event {entityId = cartId, ownerId = user.sub})]
    Nothing -> do
      anonymousId <- Decider.generateUuid
      Decider.acceptNew [CartCreated (CartCreated.Event {entityId = cartId, ownerId = Uuid.toText anonymousId})]

type instance EntityOf CreateCart = CartEntity
type instance TransportsOf CreateCart = '[WebTransport]

deriveCommand ''CreateCart
```

### `src/Shop/Cart/Queries/CartSummary.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Queries/CartSummary.hs"
module Shop.Cart.Queries.CartSummary (CartSummary (..), canAccess, canView) where

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
    let count = 0
    Update CartSummary
      { cartSummaryId = cart.cartId
      , ownerId = cart.ownerId
      , itemCount = count
      , isEmpty = count == 0
      }
```

### `src/Shop/Cart/Service.hs` — create

<!-- complete-file -->
```haskell title="src/Shop/Cart/Service.hs"
module Shop.Cart.Service (service) where

import Core
import Service qualified
import Shop.Cart.Commands.CreateCart (CreateCart)

service :: Service _ _
service = Service.new
  |> Service.command @CreateCart
```

The [first-cart archive](/examples/mug-shop-first-cart.tar.gz) remains a convenient comparison checkpoint, but it is not needed to obtain these files. The tests in that archive are introduced as authored evidence in the [testing lesson](/build/testing/).

## Build it and make a request

From the `mug-shop` project root:

```sh
neo build
neo run
```

In another terminal, request a Cart:

```sh
curl -i http://localhost:8080/commands/create-cart \
  -H 'Content-Type: application/json' \
  --data '[]'
```

Expect HTTP 200 and a JSON object containing `entityId`. Keep that UUID. The `[]` body is the encoding of this fieldless command.

Read the view:

```sh
curl http://localhost:8080/queries/cart-summary
```

Find the row whose `cartSummaryId` matches your `entityId`. It should have `itemCount: 0` and `isEmpty: true`. The response is a page containing `items`, `total`, `hasMore`, and `effectiveLimit`.

The read model updates asynchronously. Repeat the read briefly if your row has not appeared. Resubmitting the creation command would create another Cart, not refresh the original one.

## Keep evidence you can run again

The [testing lesson](/build/testing/) adds a unit spec for the accepted `CartCreated` event and the refusal of an existing Cart, plus an HTTP scenario that waits for the empty summary. Until then, the build, server response, and query response above are the runnable first checkpoint. The optional archive contains those public test sources for comparison.

You have created a Cart, not an accepted order. No price, payment, or fulfilment promise appears in the model. Ask your agent to point to the fact behind each proposed claim.

## Try a variation

Create two Carts and identify both summaries. Then send malformed JSON, such as a body containing only `{`. What should remain unchanged after that refused request?

<details>
<summary>Suggested reasoning and checks</summary>

Two successful requests should return different IDs and acquire separate empty summaries. Malformed JSON should produce a client error without an accepted creation response. An empty Cart is a valid created entity, distinct from a missing Cart. Restarting this nonpersistent application starts a fresh exercise.

</details>

Next: [explore your Cart in the visual IDE](/getting-started/visual-ide/), running `neo ide` from this same project. Then [add a new command](/build/commands-and-events/).
