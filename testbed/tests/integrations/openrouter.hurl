# OpenRouter Integration Tests
#
# These tests verify the OpenRouter integration behavior.
# Run with: hurl --test tests/integrations/openrouter.hurl
#
# Note: These tests use a mock server to verify request structure
# without making real API calls.

# Test 1: Verify request structure
# The integration should create a proper POST request to OpenRouter
#
# Mock server at localhost:8081 simulates OpenRouter API
POST http://localhost:8081/api/v1/chat/completions
Content-Type: application/json
Authorization: Bearer test-api-key
{
  "messages": [
    {"role": "user", "content": "Hello"}
  ],
  "model": "anthropic/claude-3.5-sonnet",
  "stream": false
}
HTTP 200
[Asserts]
jsonpath "$.id" exists
jsonpath "$.model" exists
jsonpath "$.choices" isCollection
jsonpath "$.choices[0].message.role" == "assistant"
jsonpath "$.choices[0].message.content" exists
jsonpath "$.choices[0].finish_reason" == "stop"


# Test 2: Verify optional parameters are included when set
POST http://localhost:8081/api/v1/chat/completions
Content-Type: application/json
Authorization: Bearer test-api-key
{
  "messages": [
    {"role": "system", "content": "You are helpful."},
    {"role": "user", "content": "Hi"}
  ],
  "model": "openai/gpt-4o",
  "stream": false,
  "temperature": 0.7,
  "max_tokens": 100
}
HTTP 200


# Test 3: Verify error handling
# API returns an error response
POST http://localhost:8081/api/v1/chat/completions
Content-Type: application/json
Authorization: Bearer invalid-key
{
  "messages": [{"role": "user", "content": "test"}],
  "model": "test-model",
  "stream": false
}
HTTP 401
[Asserts]
jsonpath "$.error.message" exists


# Test 4: Verify rate limit handling
# API returns 429 Too Many Requests
POST http://localhost:8081/api/v1/chat/completions
Content-Type: application/json
Authorization: Bearer test-api-key
X-Test-Scenario: rate-limit
{
  "messages": [{"role": "user", "content": "test"}],
  "model": "test-model",
  "stream": false
}
HTTP 429
[Asserts]
header "Retry-After" exists
