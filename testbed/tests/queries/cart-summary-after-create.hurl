# Cart Lifecycle - Create and Verify Query State
# Tests the full flow: create cart -> verify it appears in query with correct values

# Step 1: Create a new cart
POST http://localhost:8080/commands/CreateCart
[]

HTTP/1.1 200

[Captures]
new_cart_id: jsonpath "$.entityId"

[Asserts]
jsonpath "$.entityId" exists

# Step 2: Query cart summaries and find our new cart
# Use retry to handle eventual consistency (query subscriber processes async)
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Our cart must exist in the results
jsonpath "$[?(@.cartSummaryId == '{{new_cart_id}}')]" count == 1

# Verify the cart has correct initial state:
# - itemCount should be 0 (newly created, no items)
# - isEmpty should be true
jsonpath "$[?(@.cartSummaryId == '{{new_cart_id}}')].itemCount" nth 0 == 0
jsonpath "$[?(@.cartSummaryId == '{{new_cart_id}}')].isEmpty" nth 0 == true

# Verify structure of the matched cart
jsonpath "$[?(@.cartSummaryId == '{{new_cart_id}}')].cartSummaryId" nth 0 == {{new_cart_id}}
