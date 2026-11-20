POST http://localhost:8080/commands/create-protected
[]

HTTP/1.1 200
[Captures]
protected_id: jsonpath "$.entityId"
[Asserts]
jsonpath "$.tag" == "Accepted"

GET http://localhost:8080/queries/protected-view
[Options]
retry: 10
retry-interval: 100

HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')]" count == 1
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].firstSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].firstSecret.contents.name" nth 0 == "Álvaro ñ 😀"
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].firstSecret.contents.aliases[0]" nth 0 == null
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].firstSecret.contents.aliases[1]" nth 0 == "café"
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].secondSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].mappedSecret.tag" nth 0 == "Available"
jsonpath "$.items[?(@.protectedId == '{{protected_id}}')].mappedSecret.contents.name" nth 0 == "Álvaro ñ 😀"
jsonpath "$.effectiveLimit" == 100
jsonpath "$.hasMore" == false
