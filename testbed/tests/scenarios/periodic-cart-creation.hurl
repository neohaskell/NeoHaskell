# Periodic Cart Creation - Inbound Timer Integration Test
# Verifies that the timer-based inbound integration creates carts periodically
# Timer is set to 3 seconds in testbed

# Step 1: Get initial cart count
GET http://localhost:8080/queries/cart-summary
[Options]
retry: 3
retry-interval: 100

HTTP/1.1 200
Content-Type: application/json

[Captures]
initial_count: jsonpath "$" count

# Step 2: Wait for timer to fire (at least 3 seconds)
# Then query again and verify a new cart was created
GET http://localhost:8080/queries/cart-summary
[Options]
delay: 4000
retry: 5
retry-interval: 500

HTTP/1.1 200
Content-Type: application/json

[Captures]
first_count: jsonpath "$" count

[Asserts]
# Cart count should have increased by at least 1
jsonpath "$" count > {{initial_count}}

# Step 3: Wait for another timer tick and verify another cart
GET http://localhost:8080/queries/cart-summary
[Options]
delay: 4000
retry: 5
retry-interval: 500

HTTP/1.1 200
Content-Type: application/json

[Captures]
second_count: jsonpath "$" count

[Asserts]
# Should have increased again (2nd timer tick)
jsonpath "$" count > {{first_count}}
