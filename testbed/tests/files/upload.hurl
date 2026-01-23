# File Upload Endpoint Tests
# POST /files/upload - Multipart file upload
#
# These tests verify the file upload endpoint behavior.
# Note: Hurl sends the full relative path as the filename, which is valid behavior.

# =============================================================================
# Test 1: Successful file upload with text file
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/hello.txt; text/plain

HTTP 200
[Captures]
file_ref_1: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "fixtures/hello.txt"
jsonpath "$.contentType" == "text/plain"
jsonpath "$.sizeBytes" == 12
jsonpath "$.expiresAt" exists


# =============================================================================
# Test 2: Successful upload with different content type (image)
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/crlf-test.png; image/png

HTTP 200
[Captures]
file_ref_2: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "fixtures/crlf-test.png"
jsonpath "$.contentType" == "image/png"
jsonpath "$.sizeBytes" == 70
jsonpath "$.expiresAt" exists


# =============================================================================
# Test 3: Successful upload with JSON file
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/data.json; application/json

HTTP 200
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "fixtures/data.json"
jsonpath "$.contentType" == "application/json"
jsonpath "$.sizeBytes" == 15


# =============================================================================
# Test 4: Upload with binary content (application/octet-stream)
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/binary.bin; application/octet-stream

HTTP 200
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "fixtures/binary.bin"
jsonpath "$.contentType" == "application/octet-stream"
jsonpath "$.sizeBytes" == 4


# =============================================================================
# Test 5: Each upload generates unique FileRef
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/unique1.txt; text/plain

HTTP 200
[Captures]
unique_ref_1: jsonpath "$.fileRef"

POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/unique2.txt; text/plain

HTTP 200
[Captures]
unique_ref_2: jsonpath "$.fileRef"
[Asserts]
# The two refs should be different
jsonpath "$.fileRef" != "{{unique_ref_1}}"
