POST http://localhost:8080/commands/initialize-stock
{
  "productId": "11111111-1111-1111-1111-111111111111",
  "available": 10
}

HTTP/1.1 200
[Captures]
stock_id: jsonpath "$.entityId"

POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
[Captures]
cart_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/cart-stock-activity
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$.items[0].cartSeen" == true
jsonpath "$.items[0].stockSeen" == true
jsonpath "$.items[0].cartItemCount" == 0
jsonpath "$.items[0].stockReserved" == 0

POST http://localhost:8080/commands/add-item
{
  "cartId": "{{cart_id}}",
  "stockId": "{{stock_id}}",
  "quantity": 1
}

HTTP/1.1 200

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 1

GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 1

GET http://localhost:8080/queries/cart-stock-activity

HTTP/1.1 200
[Asserts]
jsonpath "$.items[0].cartSeen" == true
jsonpath "$.items[0].stockSeen" == true
jsonpath "$.items[0].cartItemCount" == 0
jsonpath "$.items[0].stockReserved" == 0
