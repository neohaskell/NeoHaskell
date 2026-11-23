POST http://localhost:8080/commands/record-reading
Content-Type: application/json
{"firstReading": 327}

HTTP 200
[Captures]
meter_id: jsonpath "$.entityId"
[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://localhost:8080/queries/meter-reading/{{meter_id}}
[Options]
retry: 20
retry-interval: 100

HTTP 200
[Asserts]
jsonpath "$.reading" == 327
