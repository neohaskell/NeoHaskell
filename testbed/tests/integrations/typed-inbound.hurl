# One typed inbound worker emits commands for the cart and stock services.

GET http://localhost:8080/queries/cart-summary
HTTP/1.1 200
[Captures]
initial_carts: jsonpath "$.items[?(@.ownerId == 'typed-inbound-example')]" count

GET http://localhost:8080/queries/stock-level
HTTP/1.1 200
[Captures]
initial_stocks: jsonpath "$.items[?(@.available == 579)]" count

GET http://localhost:8080/queries/cart-summary
[Options]
delay: 4000
retry: 5
retry-interval: 500
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.ownerId == 'typed-inbound-example')]" count > {{initial_carts}}

GET http://localhost:8080/queries/stock-level
[Options]
retry: 5
retry-interval: 500
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.available == 579)]" count > {{initial_stocks}}
