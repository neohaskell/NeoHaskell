# RegisterCartByKey - deterministic UUID v5 entity identity (issue #596)
#
# Proves end-to-end, through the real HTTP transport and event store, that a
# natural key IS the identity: the same key always resolves to the same cart
# stream, so a repeat registration is rejected as a duplicate rather than
# creating a second cart.
#
# The key is minted fresh per run (a throwaway cart's random id) so the suite
# stays re-runnable against a persistent store. The cross-implementation proof
# that the derivation is the exact RFC 4122 v5 value lives in two places that a
# per-run key cannot reach: the literal owner id asserted in step 4, and the
# published test vectors in UuidSpec.

# Step 0: mint a per-run unique natural key.
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200

[Captures]
run_key: jsonpath "$.entityId"

# Step 1: register a cart under that key.
POST http://localhost:8080/commands/register-cart-by-key
{
  "key": "repo/{{run_key}}"
}

HTTP/1.1 200
Content-Type: application/json

[Captures]
derived_id: jsonpath "$.entityId"

[Asserts]
# The id is not random: it is a well-formed v5 UUID — version nibble 5 and the
# RFC 4122 variant bits — so the derivation survived the whole round trip.
jsonpath "$.entityId" matches /^[0-9a-f]{8}-[0-9a-f]{4}-5[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/

# Step 2: the SAME key again is rejected — it routed to the existing stream.
# If the key had derived a different id, this would have created a second cart
# and returned 200 instead.
POST http://localhost:8080/commands/register-cart-by-key
{
  "key": "repo/{{run_key}}"
}

HTTP/1.1 400
Content-Type: application/json

[Asserts]
jsonpath "$.reason" == "Cart already registered for this key!"

# Step 3: a DIFFERENT key lands on a different stream — distinct keys must not
# collide into one cart.
POST http://localhost:8080/commands/register-cart-by-key
{
  "key": "other/{{run_key}}"
}

HTTP/1.1 200

[Asserts]
jsonpath "$.entityId" matches /^[0-9a-f]{8}-[0-9a-f]{4}-5[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/
jsonpath "$.entityId" != "{{derived_id}}"

# Step 4: the owner id derived inside `decide` via
# Decider.generateDeterministicUuid is the exact RFC 4122 v5 value for
# ("anonymous" under the command's owner namespace) — asserted literally, so a
# drift in the derivation fails here rather than silently changing identities.
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{derived_id}}')]" count == 1
jsonpath "$.items[?(@.cartSummaryId == '{{derived_id}}')].ownerId" nth 0 == "6b833e0a-aa86-5500-8aa0-7308c7fef74a"
