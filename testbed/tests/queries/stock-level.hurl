# Stock Level Query - Public Access
# Verifies public query endpoint returns a valid JSON array without authentication

GET http://localhost:8080/queries/stock-level

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Response must be a paginated object with items array
jsonpath "$.items" isCollection
jsonpath "$.total" isInteger
jsonpath "$.hasMore" isBoolean
jsonpath "$.effectiveLimit" isInteger
