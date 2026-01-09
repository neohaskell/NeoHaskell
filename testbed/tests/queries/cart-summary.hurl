# Cart Summary Query - Basic Functionality
# Verifies endpoint returns a valid JSON array

GET http://localhost:8080/queries/cart-summary

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Response must be a JSON array (even if empty)
jsonpath "$" isCollection
