# Smoke test — verifies the HTTP server is up and the happy path of the
# simplest command works end to end.

POST http://localhost:8080/commands/create-counter
Content-Type: application/json
{
  "label": "smoke-test"
}

HTTP 200
[Asserts]
jsonpath "$.entityId" exists
jsonpath "$.entityId" matches /^[0-9a-f-]{36}$/
