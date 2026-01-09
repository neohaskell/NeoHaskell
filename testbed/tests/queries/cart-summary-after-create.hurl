# Test flow: Create a cart, then verify it appears in cart summary query

# Step 1: Create a cart
POST http://localhost:8080/commands/CreateCart
[]

HTTP/1.1 200

# Step 2: Query cart summaries - should include the newly created cart
GET http://localhost:8080/queries/cart-summary

HTTP/1.1 200
Content-Type: application/json
