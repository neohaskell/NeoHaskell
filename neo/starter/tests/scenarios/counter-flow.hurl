POST http://localhost:8080/commands/create-counter
Content-Type: application/json
{
  "label": "downloads"
}

HTTP 200
[Captures]
counter_id: jsonpath "$.entityId"
[Asserts]
jsonpath "$.entityId" matches /^[0-9a-f-]{36}$/

POST http://localhost:8080/commands/increment-counter
Content-Type: application/json
{
  "entityId": "{{counter_id}}",
  "amount": 1
}

HTTP 200

POST http://localhost:8080/commands/increment-counter
Content-Type: application/json
{
  "entityId": "{{counter_id}}",
  "amount": 1
}

HTTP 200

POST http://localhost:8080/commands/decrement-counter
Content-Type: application/json
{
  "entityId": "{{counter_id}}",
  "amount": 1
}

HTTP 200

GET http://localhost:8080/queries/counter-view
[Options]
retry: 10
retry-interval: 200


HTTP 200
[Asserts]
jsonpath "$.items[?(@.counterId == '{{counter_id}}')].value" nth 0 == 1
jsonpath "$.items[?(@.counterId == '{{counter_id}}')].label" nth 0 == "downloads"

POST http://localhost:8080/commands/create-counter
Content-Type: application/json
{
  "label": ""
}

HTTP 400
