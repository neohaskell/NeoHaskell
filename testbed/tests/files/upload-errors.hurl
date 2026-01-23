# File Upload Error Cases
# Tests for validation and error handling in POST /files/upload
#
# Note: Some tests require specific server configuration (e.g., maxFileSizeBytes)
# The testbed should be configured with reasonable limits for testing.

# =============================================================================
# Test 1: Missing file in multipart request
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary

HTTP 400
[Asserts]
jsonpath "$.error" exists


# =============================================================================
# Test 2: Invalid multipart format (plain JSON instead of multipart)
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: application/json
{
  "filename": "test.txt",
  "content": "hello"
}

HTTP 400
[Asserts]
jsonpath "$.error" exists


# =============================================================================
# Test 3: Empty request body
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary

HTTP 400
[Asserts]
jsonpath "$.error" exists


# =============================================================================
# Test 4: Empty boundary (security - prevents DoS via infinite split)
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: multipart/form-data; boundary=

HTTP 400
[Asserts]
jsonpath "$.error" exists


# =============================================================================
# Test 5: Boundary too long (security - RFC 2046 limit is 70 chars)
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: multipart/form-data; boundary=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

HTTP 400
[Asserts]
jsonpath "$.error" exists


# =============================================================================
# Test 6: Whitespace-only boundary (treated as empty)
# =============================================================================
POST http://localhost:8080/files/upload
Content-Type: multipart/form-data; boundary="   "

HTTP 400
[Asserts]
jsonpath "$.error" exists
