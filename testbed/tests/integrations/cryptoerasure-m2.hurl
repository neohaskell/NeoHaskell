GET http://127.0.0.1:8181/fixture/jwt

HTTP/1.1 200
[Captures]
jwt: jsonpath "$.accessToken"

POST http://127.0.0.1:8081/commands/create-owned-protected
Authorization: Bearer {{jwt}}
[]

HTTP/1.1 200
[Captures]
case_id: jsonpath "$.entityId"
[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 20
retry-interval: 100ms

HTTP/1.1 200
[Captures]
reference: jsonpath "$.items[?(@.caseId == '{{case_id}}')].firstReference" nth 0
[Asserts]
jsonpath "$.items[?(@.caseId == '{{case_id}}')].firstSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].secondSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].erasureStatus" nth 0 == "not-requested"

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RequestFirstErasure",
  "caseId": "{{case_id}}",
  "reference": "not-the-owned-reference",
  "rejectResult": false,
  "simulateUnknownReference": false,
  "pauseAfterCommit": false
}

HTTP/1.1 400
[Asserts]
jsonpath "$.tag" == "Rejected"

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RequestFirstErasure",
  "caseId": "{{case_id}}",
  "reference": "{{reference}}",
  "rejectResult": true,
  "simulateUnknownReference": false,
  "pauseAfterCommit": false
}

HTTP/1.1 200
[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 20
retry-interval: 100ms

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.caseId == '{{case_id}}')].firstSecret.tag" nth 0 == "Erased"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].secondSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].erasureStatus" nth 0 == "pending"

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RetryFirstErasure",
  "caseId": "{{case_id}}"
}

HTTP/1.1 200
[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 100ms

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.caseId == '{{case_id}}')].firstSecret.tag" nth 0 == "Erased"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].secondSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].erasureStatus" nth 0 == "completed"
jsonpath "$.items[?(@.caseId == '{{case_id}}')].wasAlreadyErased" nth 0 == true

POST http://127.0.0.1:8081/commands/create-owned-protected
Authorization: Bearer {{jwt}}
[]

HTTP/1.1 200
[Captures]
other_case_id: jsonpath "$.entityId"

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 20
retry-interval: 100ms

HTTP/1.1 200
[Captures]
other_reference: jsonpath "$.items[?(@.caseId == '{{other_case_id}}')].firstReference" nth 0

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RequestFirstErasure",
  "caseId": "{{other_case_id}}",
  "reference": "{{reference}}",
  "rejectResult": false,
  "simulateUnknownReference": false,
  "pauseAfterCommit": false
}

HTTP/1.1 400
[Asserts]
jsonpath "$.tag" == "Rejected"

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RequestFirstErasure",
  "caseId": "{{other_case_id}}",
  "reference": "{{other_reference}}",
  "rejectResult": false,
  "simulateUnknownReference": false,
  "pauseAfterCommit": false
}

HTTP/1.1 200

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 100ms

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.caseId == '{{other_case_id}}')].firstSecret.tag" nth 0 == "Erased"
jsonpath "$.items[?(@.caseId == '{{other_case_id}}')].secondSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.caseId == '{{other_case_id}}')].erasureStatus" nth 0 == "completed"
jsonpath "$.items[?(@.caseId == '{{other_case_id}}')].wasAlreadyErased" nth 0 == false

POST http://127.0.0.1:8081/commands/create-owned-protected
Authorization: Bearer {{jwt}}
[]

HTTP/1.1 200
[Captures]
failure_case_id: jsonpath "$.entityId"

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 20
retry-interval: 100ms

HTTP/1.1 200
[Captures]
failure_reference: jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].firstReference" nth 0

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RequestFirstErasure",
  "caseId": "{{failure_case_id}}",
  "reference": "{{failure_reference}}",
  "rejectResult": false,
  "simulateUnknownReference": true,
  "pauseAfterCommit": false
}

HTTP/1.1 200

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 100ms

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].erasureStatus" nth 0 == "failed"
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].failureReason" nth 0 == "unknown reference"
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].firstSecret.tag" nth 0 == "Available"

POST http://127.0.0.1:8081/commands/request-erasure
Authorization: Bearer {{jwt}}
{
  "tag": "RetryFirstErasure",
  "caseId": "{{failure_case_id}}"
}

HTTP/1.1 200

GET http://127.0.0.1:8081/queries/owned-protected-view
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 100ms

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].erasureStatus" nth 0 == "completed"
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].wasAlreadyErased" nth 0 == false
jsonpath "$.items[?(@.caseId == '{{failure_case_id}}')].firstSecret.tag" nth 0 == "Erased"
