# File Download Endpoint Tests
# GET /files/:fileRef - Download uploaded file
#
# These tests verify the file download endpoint behavior.

# =============================================================================
# Setup: Upload a file first to get a valid FileRef
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,download-test.txt; text/plain; SGVsbG8gV29ybGQh

HTTP 200
[Captures]
download_file_ref: jsonpath "$.fileRef"


# =============================================================================
# Test 1: Successful download of uploaded file
# =============================================================================
GET http://localhost:8080/files/{{download_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "text/plain"
header "Content-Disposition" contains "download-test.txt"
# Base64 "SGVsbG8gV29ybGQh" = "Hello World!"
body == "Hello World!"


# =============================================================================
# Test 2: Download binary file (PNG)
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,binary.png; image/png; iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==

HTTP 200
[Captures]
binary_file_ref: jsonpath "$.fileRef"

GET http://localhost:8080/files/{{binary_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "image/png"
header "Content-Disposition" contains "binary.png"
# Body should be the decoded PNG bytes
bytes count > 0


# =============================================================================
# Test 3: Download non-existent file returns 404
# =============================================================================
GET http://localhost:8080/files/file_nonexistent_00000000-0000-0000-0000-000000000000

HTTP 404
[Asserts]
jsonpath "$.error" contains "not found"
