# Internal Logs - Basic Endpoint
# Verifies the endpoint returns a valid JSON response with logs array and count

GET http://localhost:8080/internal/logs

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Response must have logs array
jsonpath "$.logs" isCollection
# Response must have count field
jsonpath "$.count" isInteger
# Count must be non-negative
jsonpath "$.count" >= 0


# Internal Logs - Limit Parameter
# Verifies the limit query parameter is respected

GET http://localhost:8080/internal/logs?limit=5

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.logs" isCollection
jsonpath "$.count" isInteger
jsonpath "$.count" <= 5


# Internal Logs - Since Parameter
# Verifies the since query parameter filters logs

GET http://localhost:8080/internal/logs?since=2099-01-01T00:00:00Z

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Far future timestamp should return no logs
jsonpath "$.logs" isCollection
jsonpath "$.count" == 0


# Internal Logs - Log Entry Structure
# Verifies that log entries have the expected fields
# The framework logs during startup, so there should be entries

GET http://localhost:8080/internal/logs?limit=1
[Options]
retry: 5
retry-interval: 500

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Should have at least one log from framework startup
jsonpath "$.count" >= 1
# First log entry should have all required fields
jsonpath "$.logs[0].logId" isString
jsonpath "$.logs[0].timestamp" isString
jsonpath "$.logs[0].level" isString
jsonpath "$.logs[0].message" isString


# Internal Logs - Exact Limit
# When there are multiple logs, limit=1 should return exactly 1

GET http://localhost:8080/internal/logs?limit=1
[Options]
retry: 5
retry-interval: 500

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.count" == 1
jsonpath "$.logs" count == 1
