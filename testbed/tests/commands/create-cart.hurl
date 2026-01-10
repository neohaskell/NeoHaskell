# CreateCart Command - Happy Path
# Verifies command execution and response structure

POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Response must have entityId field
jsonpath "$.entityId" exists
# entityId must be a valid UUID format (lowercase hex with dashes)
jsonpath "$.entityId" isString
jsonpath "$.entityId" matches /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/
