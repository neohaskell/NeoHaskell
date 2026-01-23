# File Download Endpoint Tests
# GET /files/:fileRef - Download uploaded file
#
# These tests verify the file download endpoint behavior.

# =============================================================================
# Setup: Upload a file first to get a valid FileRef
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/hello.txt; text/plain

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
header "Content-Disposition" contains "hello.txt"
body == "Hello World!"


# =============================================================================
# Test 2: Download binary file (PNG)
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/crlf-test.png; image/png

HTTP 200
[Captures]
binary_file_ref: jsonpath "$.fileRef"

GET http://localhost:8080/files/{{binary_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "image/png"
header "Content-Disposition" contains "crlf-test.png"
# Body should be the decoded PNG bytes
bytes count > 0


# =============================================================================
# Test 3: Binary file with CRLF in header (PNG) survives round-trip
# This tests the fix for issue #284 - binary files containing \r\n (CRLF)
# sequences were being corrupted by the multipart parser.
# PNG magic header is: 89 50 4E 47 0D 0A 1A 0A (contains CRLF at bytes 4-5)
# =============================================================================
POST http://localhost:8080/files/upload
[Multipart]
file: file,fixtures/crlf-test.png; image/png

HTTP 200
[Captures]
crlf_file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.filename" == "fixtures/crlf-test.png"
jsonpath "$.contentType" == "image/png"
jsonpath "$.sizeBytes" == 70

# Download and verify exact byte content
GET http://localhost:8080/files/{{crlf_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "image/png"
# SHA256 of the original 70-byte PNG file
sha256 == hex,bc09c2590d2502c8ffaf1a3c09aa89df222e03d186a8daa0c7fce6321fb6e928;


# =============================================================================
# Test 4: Download non-existent file returns 404
# =============================================================================
GET http://localhost:8080/files/file_nonexistent_00000000-0000-0000-0000-000000000000

HTTP 404
[Asserts]
jsonpath "$.error" contains "not found"
