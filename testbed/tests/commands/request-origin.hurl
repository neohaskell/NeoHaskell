POST http://127.0.0.1:8080/commands/check-request-origin
X-Forwarded-For: 198.51.100.42
User-Agent: neohaskell-request-origin-example
[]

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.tag" == "Accepted"
jsonpath "$.entityId" matches /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/
