module Testbed.Cart.Commands.RegisterCartByKey (
  RegisterCartByKey (..),
  keyNamespace,
  ownerNamespace,
  getEntityId,
  decide,
) where -- HOOK-ALLOW: module declaration `where`, not a let-substitute

import Core
import Decider qualified
import Maybe qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core (CartEntity, CartEvent (..))
import Uuid qualified


-- | Register a cart under a natural key (issue #596).
--
-- Demonstrates deterministic UUID v5 identity end to end: the same key always
-- resolves to the same cart stream, so registering it twice is rejected as a
-- duplicate instead of silently creating a second cart. This is the standard
-- event-sourcing answer to cross-aggregate uniqueness — no second store is
-- consulted, the identity IS the derivation.
data RegisterCartByKey = RegisterCartByKey
  { key :: Text
  }


-- | Collision domain for cart natural keys. Two carts registered under the
-- same key are the same cart; the same key under another namespace is not.
keyNamespace :: Uuid
keyNamespace =
  "4b0d264f-ab5e-51d1-acd5-4f55180d00a0"
    |> Uuid.fromText
    |> Maybe.getOrDie


-- | Collision domain for derived cart owners, kept separate from the key
-- namespace on purpose — the same text must not derive the same id for two
-- unrelated things.
ownerNamespace :: Uuid
ownerNamespace =
  "df334ca3-444a-5dea-ac07-3aaf4c4c84f5"
    |> Uuid.fromText
    |> Maybe.getOrDie


-- | Carts registered by key have no authenticated user, so they share one
-- stable, well-known owner — the "stable reference" half of the pattern.
anonymousOwner :: Text
anonymousOwner = "anonymous"


-- | The natural key IS the identity: the stream id is derived from it, so a
-- repeat registration routes to the existing stream rather than a new one.
-- 'getEntityId' is pure, which is exactly why the derivation has to exist as a
-- pure function ('Uuid.generateV5') and not only inside 'Decision'.
getEntityId :: RegisterCartByKey -> Maybe Uuid
getEntityId cmd =
  Uuid.generateV5 keyNamespace cmd.key |> Just


-- | Reject a key that is already registered; otherwise create the cart with a
-- deterministically derived owner.
decide :: RegisterCartByKey -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd entity _ctx = case entity of
  Just _ ->
    Decider.reject "Cart already registered for this key!"
  Nothing -> do
    ownerId <- Decider.generateDeterministicUuid ownerNamespace anonymousOwner
    Decider.acceptNew
      [ CartCreated
          { entityId = Uuid.generateV5 keyNamespace cmd.key
          , ownerId = Uuid.toText ownerId
          }
      ]


type instance EntityOf RegisterCartByKey = CartEntity


type instance TransportsOf RegisterCartByKey = '[WebTransport]


command ''RegisterCartByKey
