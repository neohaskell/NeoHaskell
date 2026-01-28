# HTTP Outbound Integration Test
# Tests the Http.Request integration pattern
#
# Prerequisites:
# - Mock HTTP server running on localhost:8080
# - Testbed configured with MOCK_API_URL=http://localhost:8080
#
# Flow:
# 1. Reset mock server state
# 2. Trigger an order shipment (creates HTTP outbound integration)
# 3. Verify mock received correct HTTP request
# 4. Verify success callback emitted correct command

# =============================================================================
# Test 1: Basic POST with JSON body
# =============================================================================

# Step 1: Reset mock server expectations
DELETE http://localhost:8080/_mock/requests
HTTP/1.1 200

# Step 2: Create an order (prerequisite)
POST http://localhost:8080/commands/create-order
{
  "customerId": "customer-123",
  "items": [
    { "productId": "product-456", "quantity": 2 }
  ]
}
HTTP/1.1 200
[Captures]
order_id: jsonpath "$.entityId"

# Step 3: Ship the order (triggers HTTP outbound to shipping provider)
POST http://localhost:8080/commands/ship-order
{
  "orderId": "{{order_id}}",
  "trackingNumber": "TRACK-789",
  "carrier": "ups"
}
HTTP/1.1 200

# Step 4: Verify mock received the HTTP request
# The integration should have POSTed to the shipping API
GET http://localhost:8080/_mock/requests
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$.requests" count >= 1
jsonpath "$.requests[0].method" == "POST"
jsonpath "$.requests[0].path" == "/v1/shipments"
jsonpath "$.requests[0].headers['X-Api-Key']" exists
jsonpath "$.requests[0].body.tracking_number" == "TRACK-789"
jsonpath "$.requests[0].body.carrier" == "ups"

# Step 5: Verify success callback emitted NotifyCustomer command
# (Would check via a query or event store inspection)
GET http://localhost:8080/queries/pending-notifications
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$[?(@.orderId == '{{order_id}}')]" count == 1
jsonpath "$[?(@.orderId == '{{order_id}}')].type" nth 0 == "NotifyCustomer"


# =============================================================================
# Test 2: Retry on 5xx errors
# =============================================================================

# Step 1: Configure mock to return 503 twice, then 200
POST http://localhost:8080/_mock/configure
{
  "path": "/v1/shipments",
  "responses": [
    { "status": 503, "body": { "error": "Service temporarily unavailable" } },
    { "status": 503, "body": { "error": "Service temporarily unavailable" } },
    { "status": 200, "body": { "tracking_url_provider": "https://track.ups.com/TRACK-999" } }
  ]
}
HTTP/1.1 200

# Step 2: Create and ship another order
POST http://localhost:8080/commands/create-order
{
  "customerId": "customer-456",
  "items": [
    { "productId": "product-789", "quantity": 1 }
  ]
}
HTTP/1.1 200
[Captures]
order_id_2: jsonpath "$.entityId"

POST http://localhost:8080/commands/ship-order
{
  "orderId": "{{order_id_2}}",
  "trackingNumber": "TRACK-999",
  "carrier": "fedex"
}
HTTP/1.1 200

# Step 3: Verify mock received 3 requests (2 retries + 1 success)
GET http://localhost:8080/_mock/requests?path=/v1/shipments
[Options]
retry: 15
retry-interval: 500

HTTP/1.1 200
[Asserts]
jsonpath "$.requests" count >= 3

# Step 4: Verify success callback was eventually called
GET http://localhost:8080/queries/pending-notifications
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$[?(@.orderId == '{{order_id_2}}')]" count == 1


# =============================================================================
# Test 3: Error callback on permanent failure
# =============================================================================

# Step 1: Configure mock to always return 400
POST http://localhost:8080/_mock/configure
{
  "path": "/v1/refunds",
  "responses": [
    { "status": 400, "body": { "error": "Invalid payment intent" } }
  ]
}
HTTP/1.1 200

# Step 2: Attempt a refund (should fail and trigger error callback)
POST http://localhost:8080/commands/refund-order
{
  "orderId": "{{order_id}}",
  "paymentIntentId": "invalid-intent",
  "amountCents": 5000
}
HTTP/1.1 200

# Step 3: Verify error was logged (via error callback command)
GET http://localhost:8080/queries/integration-errors
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$[?(@.orderId == '{{order_id}}')]" count == 1
jsonpath "$[?(@.orderId == '{{order_id}}')].type" nth 0 == "LogRefundError"


# =============================================================================
# Test 4: Environment variable expansion in auth
# =============================================================================

# This test verifies that ${VAR_NAME} patterns are expanded from environment.
# The mock server should receive the actual API key value, not the placeholder.

# Step 1: Clear mock requests
DELETE http://localhost:8080/_mock/requests
HTTP/1.1 200

# Step 2: Trigger an authenticated request
POST http://localhost:8080/commands/ship-order
{
  "orderId": "{{order_id}}",
  "trackingNumber": "TRACK-AUTH-TEST",
  "carrier": "dhl"
}
HTTP/1.1 200

# Step 3: Verify the actual API key was sent (not "${SHIPPO_API_KEY}")
GET http://localhost:8080/_mock/requests
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
# Should NOT contain the placeholder
jsonpath "$.requests[0].headers['X-Api-Key']" not contains "${"
# Should contain an actual key value (non-empty)
jsonpath "$.requests[0].headers['X-Api-Key']" matches "^.+$"
