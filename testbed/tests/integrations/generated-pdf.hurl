GET http://127.0.0.1:8181/fixture/jwt

HTTP/1.1 200

[Captures]
owner_jwt: jsonpath "$.accessToken"

GET http://127.0.0.1:8181/fixture/other-jwt

HTTP/1.1 200

[Captures]
other_jwt: jsonpath "$.accessToken"

POST http://127.0.0.1:8081/commands/request-generated-pdf
Authorization: Bearer {{owner_jwt}}
{
  "documentId": "9f002524-8d71-4a7b-b102-9c6a5cb2b92d"
}

HTTP/1.1 200

[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://127.0.0.1:8081/queries/generated-pdf-view
Authorization: Bearer {{owner_jwt}}
[Options]
retry: 60
retry-interval: 200

HTTP/1.1 200

[Captures]
file_ref: jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].fileRef" nth 0

[Asserts]
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].ownerId" nth 0 == "fixture-user"
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].status" nth 0 == "ready"
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].completedAttempts" nth 0 == 1

GET http://127.0.0.1:8081/files/{{file_ref}}
Authorization: Bearer {{owner_jwt}}

HTTP/1.1 200

[Asserts]
header "Content-Type" == "application/pdf"
header "Content-Disposition" contains "attachment;"
header "Content-Disposition" contains "generated-document.pdf"
bytes count == 51
sha256 == hex,51cf94049a39cea6282cd056762599d92e93e7b85f24cb8d41f54701c46379fb;

GET http://127.0.0.1:8081/files/{{file_ref}}

HTTP/1.1 403

GET http://127.0.0.1:8081/files/{{file_ref}}
Authorization: Bearer invalid-token

HTTP/1.1 403

GET http://127.0.0.1:8081/queries/generated-pdf-view
Authorization: Bearer {{other_jwt}}

HTTP/1.1 200

[Asserts]
jsonpath "$.items" count == 0
jsonpath "$.total" == 0

GET http://127.0.0.1:8081/files/{{file_ref}}
Authorization: Bearer {{other_jwt}}

HTTP/1.1 403

POST http://127.0.0.1:8081/commands/request-generated-pdf
Authorization: Bearer {{owner_jwt}}
{
  "documentId": "9f002524-8d71-4a7b-b102-9c6a5cb2b92d"
}

HTTP/1.1 200

[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://127.0.0.1:8081/queries/generated-pdf-view
Authorization: Bearer {{owner_jwt}}
[Options]
retry: 60
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].fileRef" nth 0 == "{{file_ref}}"
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].status" nth 0 == "ready"
jsonpath "$.items[?(@.documentId == '9f002524-8d71-4a7b-b102-9c6a5cb2b92d')].completedAttempts" nth 0 == 2
