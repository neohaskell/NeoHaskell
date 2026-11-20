POST http://127.0.0.1:8181/fixture/reset/external-effects

HTTP/1.1 200

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "{{delivered_effect_id}}",
  "description": "capture payment",
  "amount": 4200
}

HTTP/1.1 200

[Asserts]
jsonpath "$.entityId" == "{{delivered_effect_id}}"

GET http://127.0.0.1:8181/external/effects/{{delivered_effect_id}}
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200
[Asserts]
jsonpath "$.requestCount" == 2
jsonpath "$.handlerCounts.primary" == 1
jsonpath "$.handlerCounts.audit" == 1

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "{{delivered_effect_id}}",
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
jsonpath "$.items[?(@.effectId == '{{delivered_effect_id}}')].status" nth 0 == "delivered"
jsonpath "$.items[?(@.effectId == '{{delivered_effect_id}}')].externalId" nth 0 == "test-effect-{{delivered_effect_id}}"

GET http://127.0.0.1:8181/external/effects/{{delivered_effect_id}}
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.outcome" == "accepted"
jsonpath "$.httpStatus" == 201
jsonpath "$.requestCount" == 4
jsonpath "$.handlerCounts.primary" == 2
jsonpath "$.handlerCounts.audit" == 2
jsonpath "$.idempotencyKey" == "{{delivered_effect_id}}"
jsonpath "$.payload.amount" == 4200

POST http://127.0.0.1:8080/commands/request-external-effect
Content-Type: application/json

{
  "effectId": "{{failed_effect_id}}",
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
jsonpath "$.items[?(@.effectId == '{{failed_effect_id}}')].status" nth 0 == "failed"
jsonpath "$.items[?(@.effectId == '{{failed_effect_id}}')].externalId" nth 0 == null

GET http://127.0.0.1:8181/external/effects/{{failed_effect_id}}
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.outcome" == "rejected"
jsonpath "$.httpStatus" == 422
jsonpath "$.externalId" == "test-effect-{{failed_effect_id}}"
jsonpath "$.handlerCounts.primary" == 1
jsonpath "$.handlerCounts.audit" == 1
