# Stock Reservation Integration Test
# Tests the Process Manager pattern: Cart -> Stock domain coordination
#
# Flow:
# 1. Initialize stock for a product
# 2. Create a cart
# 3. Add item to cart (triggers integration)
# 4. Verify stock was reserved automatically

# Step 1: Initialize stock with 100 units available
POST http://localhost:8080/commands/initialize-stock
{
  "productId": "11111111-1111-1111-1111-111111111111",
  "available": 100
}

HTTP/1.1 200
[Captures]
stock_id: jsonpath "$.entityId"

# Step 2: Verify stock level query shows initial state
GET http://localhost:8080/queries/stock-level
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200
[Asserts]
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')]" count == 1
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 100
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 0

# Step 3: Create a cart
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
[Captures]
cart_id: jsonpath "$.entityId"

# Step 4: Add item to cart - this triggers the integration to reserve stock
POST http://localhost:8080/commands/add-item
{
  "cartId": "{{cart_id}}",
  "stockId": "{{stock_id}}",
  "quantity": 5
}

HTTP/1.1 200

# Step 5: Verify stock was reserved (integration ran)
# The Cart integration should have emitted ReserveStock command
GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
# Stock should now show 95 available, 5 reserved
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 95
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 5

# Step 6: Add more items to verify integration works multiple times
POST http://localhost:8080/commands/add-item
{
  "cartId": "{{cart_id}}",
  "stockId": "{{stock_id}}",
  "quantity": 10
}

HTTP/1.1 200

# Step 7: Verify cumulative reservation
GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
# Stock should now show 85 available, 15 reserved (5 + 10)
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 85
jsonpath "$[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 15
