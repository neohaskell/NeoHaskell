POST http://127.0.0.1:8181/fixture/reset/external-effects

HTTP/1.1 200

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "550e8400-e29b-41d4-a716-446655440810",
  "description": "capture payment",
  "amount": 4200
}

HTTP/1.1 200

[Asserts]
jsonpath "$.entityId" == "550e8400-e29b-41d4-a716-446655440810"

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "550e8400-e29b-41d4-a716-446655440810",
  "description": "capture payment",
  "amount": 4200
}

HTTP/1.1 200

GET http://127.0.0.1:8080/queries/external-effect-summary
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.effectId == '550e8400-e29b-41d4-a716-446655440810')].status" nth 0 == "delivered"
jsonpath "$.items[?(@.effectId == '550e8400-e29b-41d4-a716-446655440810')].externalId" nth 0 == "test-effect-550e8400-e29b-41d4-a716-446655440810"

GET http://127.0.0.1:8181/external/effects/550e8400-e29b-41d4-a716-446655440810

HTTP/1.1 200

[Asserts]
jsonpath "$.outcome" == "accepted"
jsonpath "$.httpStatus" == 201
jsonpath "$.requestCount" == 2
jsonpath "$.idempotencyKey" == "550e8400-e29b-41d4-a716-446655440810"
jsonpath "$.payload.amount" == 4200

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "550e8400-e29b-41d4-a716-446655440811",
  "description": "force-failure",
  "amount": 0
}

HTTP/1.1 200

GET http://127.0.0.1:8080/queries/external-effect-summary
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.effectId == '550e8400-e29b-41d4-a716-446655440811')].status" nth 0 == "failed"
jsonpath "$.items[?(@.effectId == '550e8400-e29b-41d4-a716-446655440811')].externalId" nth 0 == null

GET http://127.0.0.1:8181/external/effects/550e8400-e29b-41d4-a716-446655440811

HTTP/1.1 200

[Asserts]
jsonpath "$.outcome" == "rejected"
jsonpath "$.httpStatus" == 422
jsonpath "$.externalId" == "test-effect-550e8400-e29b-41d4-a716-446655440811"
