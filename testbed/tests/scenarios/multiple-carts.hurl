# Multiple Carts - Isolation Test
# Creates multiple carts and verifies each appears independently in query

# Step 1: Create first cart
POST http://localhost:8080/commands/CreateCart
[]

HTTP/1.1 200
[Captures]
cart_1_id: jsonpath "$.entityId"

# Step 2: Create second cart
POST http://localhost:8080/commands/CreateCart
[]

HTTP/1.1 200
[Captures]
cart_2_id: jsonpath "$.entityId"

[Asserts]
# Second cart should have different ID than first
jsonpath "$.entityId" != {{cart_1_id}}

# Step 3: Create third cart
POST http://localhost:8080/commands/CreateCart
[]

HTTP/1.1 200
[Captures]
cart_3_id: jsonpath "$.entityId"

[Asserts]
# Third cart should have different ID than first two
jsonpath "$.entityId" != {{cart_1_id}}
jsonpath "$.entityId" != {{cart_2_id}}

# Step 4: Query and verify all three carts exist independently
GET http://localhost:8080/queries/cart-summary

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# All three carts should exist in results
jsonpath "$[?(@.cartSummaryId == '{{cart_1_id}}')]" count == 1
jsonpath "$[?(@.cartSummaryId == '{{cart_2_id}}')]" count == 1
jsonpath "$[?(@.cartSummaryId == '{{cart_3_id}}')]" count == 1

# All should be empty (initial state)
jsonpath "$[?(@.cartSummaryId == '{{cart_1_id}}')].isEmpty" nth 0 == true
jsonpath "$[?(@.cartSummaryId == '{{cart_2_id}}')].isEmpty" nth 0 == true
jsonpath "$[?(@.cartSummaryId == '{{cart_3_id}}')].isEmpty" nth 0 == true

# All should have zero items
jsonpath "$[?(@.cartSummaryId == '{{cart_1_id}}')].itemCount" nth 0 == 0
jsonpath "$[?(@.cartSummaryId == '{{cart_2_id}}')].itemCount" nth 0 == 0
jsonpath "$[?(@.cartSummaryId == '{{cart_3_id}}')].itemCount" nth 0 == 0
