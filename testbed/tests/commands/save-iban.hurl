POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200

[Captures]
cart_id: jsonpath "$.entityId"

POST http://localhost:8080/commands/save-iban
{
  "cartId": "{{cart_id}}",
  "iban": "DE89370400440532013000"
}

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.entityId" == "{{cart_id}}"

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 5
retry-interval: 100

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].iban" nth 0 == "DE89370400440532013000"
