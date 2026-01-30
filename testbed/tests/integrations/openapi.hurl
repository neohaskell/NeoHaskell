# OpenAPI Integration Tests
# Verifies that OpenAPI spec generation and documentation endpoints work correctly

# Test 1: /openapi.json returns valid OpenAPI 3.0 spec
GET http://localhost:8080/openapi.json

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Verify OpenAPI version
jsonpath "$.openapi" == "3.0.0"
# Verify API info from testbed configuration
jsonpath "$.info.title" == "Testbed API"
jsonpath "$.info.version" == "1.0.0"
jsonpath "$.info.description" exists
# Verify paths are generated
jsonpath "$.paths" exists
# Verify at least one command path exists (e.g., /commands/CreateCart)
jsonpath "$.paths" count > 0


# Test 2: /openapi.yaml returns valid YAML spec
GET http://localhost:8080/openapi.yaml

HTTP/1.1 200
Content-Type: application/x-yaml

[Asserts]
# Verify YAML starts with openapi version
body contains "openapi:"
body contains "3.0.0"
# Verify API info is present
body contains "title: Testbed API"
body contains "version: 1.0.0"
# Verify paths section exists
body contains "paths:"


# Test 3: /docs returns Scalar documentation HTML
GET http://localhost:8080/docs

HTTP/1.1 200
Content-Type: text/html; charset=utf-8

[Asserts]
# Verify HTML structure
body contains "<!DOCTYPE html>"
body contains "<html"
# Verify Scalar is loaded
body contains "scalar"
body contains "/openapi.json"
# Verify API title in page
body contains "Testbed API"
