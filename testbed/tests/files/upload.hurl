# File Upload Endpoint Tests
# POST /files/upload - Multipart file upload
#
# These tests verify the file upload endpoint behavior.

# =============================================================================
# Test 1: Successful file upload with text file
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,hello.txt; text/plain; SGVsbG8gV29ybGQh

HTTP 200
[Captures]
file_ref_1: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "hello.txt"
jsonpath "$.contentType" == "text/plain"
jsonpath "$.sizeBytes" == 12
jsonpath "$.expiresAt" exists


# =============================================================================
# Test 2: Successful upload with different content type (image)
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
# PNG magic bytes (minimal valid PNG header for testing)
file: file,test.png; image/png; iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==

HTTP 200
[Captures]
file_ref_2: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "test.png"
jsonpath "$.contentType" == "image/png"
jsonpath "$.sizeBytes" > 0
jsonpath "$.expiresAt" exists


# =============================================================================
# Test 3: Successful upload with JSON file
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,data.json; application/json; eyJrZXkiOiJ2YWx1ZSJ9

HTTP 200
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "data.json"
jsonpath "$.contentType" == "application/json"
jsonpath "$.sizeBytes" == 15


# =============================================================================
# Test 4: Upload with binary content (application/octet-stream)
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,binary.bin; application/octet-stream; AQIDBA==

HTTP 200
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "binary.bin"
jsonpath "$.contentType" == "application/octet-stream"
jsonpath "$.sizeBytes" == 4


# =============================================================================
# Test 5: Each upload generates unique FileRef
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,unique1.txt; text/plain; dGVzdDE=

HTTP 200
[Captures]
unique_ref_1: jsonpath "$.fileRef"

POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,unique2.txt; text/plain; dGVzdDI=

HTTP 200
[Captures]
unique_ref_2: jsonpath "$.fileRef"
[Asserts]
# The two refs should be different
jsonpath "$.fileRef" != "{{unique_ref_1}}"
