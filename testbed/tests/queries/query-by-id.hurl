POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200

[Captures]
cart_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/cart-summary/{{cart_id}}
[Options]
retry: 5
retry-interval: 100ms

HTTP/1.1 200
Content-Type: application/json

[Captures]
owner_id: jsonpath "$.ownerId"

[Asserts]
jsonpath "$.cartSummaryId" == "{{cart_id}}"
jsonpath "$.ownerId" matches /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/
jsonpath "$.itemCount" == 0
jsonpath "$.isEmpty" == true
jsonpath "$.iban" == null
jsonpath "$.items" not exists

GET http://localhost:8080/queries/cart-summary/{{cart_id}}
[Query]
q: .itemCount > 0
limit: 0
offset: 1

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.cartSummaryId" == "{{cart_id}}"
jsonpath "$.ownerId" == "{{owner_id}}"
jsonpath "$.itemCount" == 0
jsonpath "$.isEmpty" == true
jsonpath "$.iban" == null
jsonpath "$.items" not exists

GET http://localhost:8080/queries/cart-summary/00000000-0000-0000-0000-000000000000

HTTP/1.1 404
Content-Type: application/json

[Asserts]
jsonpath "$.error" == "Query object not found"

GET http://localhost:8080/queries/cart-summary/not-a-uuid

HTTP/1.1 400
Content-Type: application/json

[Asserts]
jsonpath "$.error" == "Invalid query ID"
