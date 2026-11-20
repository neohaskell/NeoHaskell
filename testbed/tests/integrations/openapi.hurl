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
# Verify command paths exist
jsonpath "$.paths['/commands/create-cart']" exists
jsonpath "$.paths['/commands/save-iban']" exists
jsonpath "$.paths['/queries/cart-summary'].get.responses['200'].content['application/json'].schema.properties.items.items.properties.cartSummaryId" exists
jsonpath "$.paths['/queries/cart-summary'].get.responses['200'].content['application/json'].schema.properties.items.items.properties.items" not exists
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters" count == 1
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters[0].name" == "id"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters[0].in" == "path"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters[0].required" == true
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters[0].schema.type" == "string"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.parameters[0].schema.format" == "uuid"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.type" == "object"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.*" count == 5
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.cartSummaryId.type" == "string"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.ownerId.type" == "string"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.itemCount.type" == "integer"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.isEmpty.type" == "boolean"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.iban.type" == "string"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.properties.iban.nullable" == true
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" count == 4
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" includes "cartSummaryId"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" includes "ownerId"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" includes "itemCount"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" includes "isEmpty"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['200'].content['application/json'].schema.required" not includes "iban"
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['400']" exists
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['401']" exists
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['403']" exists
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['404']" exists
jsonpath "$.paths['/queries/cart-summary/{id}'].get.responses['500']" exists
jsonpath "$.paths['/commands/create-cart'].post.responses['200'].content['application/json'].schema.oneOf[0].properties.entityId" exists
jsonpath "$.paths['/commands/create-cart'].post.responses['200'].content['application/json'].schema.oneOf[0].properties.contents" not exists
jsonpath "$.paths['/commands/create-cart'].post.responses['200'].content['application/json'].schema.oneOf[0].properties.events" not exists


# Test 2: /openapi.yaml returns valid YAML spec
GET http://localhost:8080/openapi.yaml

HTTP/1.1 200
Content-Type: application/yaml

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
