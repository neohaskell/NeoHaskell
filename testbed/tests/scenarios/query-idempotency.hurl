# Cart Summary Query - Idempotency
# Multiple queries should return consistent results

# Create a cart for a known data point
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
[Captures]
known_cart_id: jsonpath "$.entityId"

# Query 1: Get baseline (with retry for eventual consistency)
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200
[Captures]
query_1_count: jsonpath "$" count

[Asserts]
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')]" count == 1

# Query 2: Should have same or more carts (never fewer, data is append-only)
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200

[Asserts]
jsonpath "$" count >= {{query_1_count}}
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')]" count == 1
# Values should be unchanged
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')].isEmpty" nth 0 == true
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')].itemCount" nth 0 == 0

# Query 3: One more time to confirm consistency
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200

[Asserts]
jsonpath "$" count >= {{query_1_count}}
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')]" count == 1
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')].isEmpty" nth 0 == true
jsonpath "$[?(@.cartSummaryId == '{{known_cart_id}}')].itemCount" nth 0 == 0
