POST http://127.0.0.1:8181/fixture/reset/oauth2

HTTP/1.1 200

GET http://127.0.0.1:8181/fixture/jwt

HTTP/1.1 200

[Captures]
jwt: jsonpath "$.accessToken"

GET http://127.0.0.1:8081/connect/test-provider
Authorization: Bearer {{jwt}}

HTTP/1.1 302

[Captures]
authorize_url: header "Location"

[Asserts]
header "Referrer-Policy" == "no-referrer"

GET {{authorize_url}}

HTTP/1.1 302

[Captures]
callback_url: header "Location"

GET {{callback_url}}

HTTP/1.1 302

[Asserts]
header "Location" == "http://127.0.0.1:8081/oauth/success"

GET http://127.0.0.1:8181/oauth/observations

HTTP/1.1 200

[Asserts]
jsonpath "$.authorizationRequests" == 1
jsonpath "$.authorizationValid" == true
jsonpath "$.tokenRequests" == 1
jsonpath "$.authorizationCodeVerified" == true
jsonpath "$.clientVerified" == true
jsonpath "$.redirectUriVerified" == true
jsonpath "$.pkceVerified" == true
jsonpath "$.tokenExchangeSucceeded" == true

GET http://127.0.0.1:8081/queries/account-connection-summary
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.userId == 'fixture-user')].status" nth 0 == "connected"
jsonpath "$.items[?(@.userId == 'fixture-user')].tokenKeyMatches" nth 0 == true
jsonpath "$.items[?(@.userId == 'fixture-user')].tokenStored" nth 0 == true

POST http://127.0.0.1:8081/disconnect/test-provider
Authorization: Bearer {{jwt}}

HTTP/1.1 200

[Asserts]
jsonpath "$.status" == "disconnected"
jsonpath "$.provider" == "test-provider"

GET http://127.0.0.1:8081/queries/account-connection-summary
Authorization: Bearer {{jwt}}
[Options]
retry: 30
retry-interval: 200

HTTP/1.1 200

[Asserts]
jsonpath "$.items[?(@.userId == 'fixture-user')].status" nth 0 == "disconnected"
jsonpath "$.items[?(@.userId == 'fixture-user')].tokenStored" nth 0 == false
