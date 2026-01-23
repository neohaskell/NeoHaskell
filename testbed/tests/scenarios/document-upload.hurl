# Document Upload Scenario (Partial - Requires File Upload Routes)
#
# This scenario demonstrates the intended file upload flow:
# 1. Upload a file -> get FileRef
# 2. Create document referencing the FileRef
# 3. Query to verify document was created
#
# NOTE: Steps requiring file upload routes are commented out until
# WebTransport is extended with POST /files/upload endpoint.
#
# Current test verifies the command endpoint properly handles missing files.

# Step 1: Upload file (REQUIRES FILE UPLOAD ROUTES)
# Uncomment when POST /files/upload is available:
#
# POST http://localhost:8080/files/upload
# [MultipartFormData]
# file: file,test.txt;text/plain;SGVsbG8gV29ybGQh
#
# HTTP/1.1 200
# [Captures]
# file_ref: jsonpath "$.fileRef"
# [Asserts]
# jsonpath "$.filename" == "test.txt"
# jsonpath "$.contentType" == "text/plain"
# jsonpath "$.sizeBytes" exists

# Step 2: Create document with file reference
# When file upload routes are wired, use {{file_ref}} captured above
# For now, test the rejection of invalid file refs:

POST http://localhost:8080/commands/create-document
{
  "documentId": "660e8400-e29b-41d4-a716-446655440001",
  "title": "Project Proposal",
  "attachment": "file_placeholder_ref"
}

HTTP/1.1 400
[Asserts]
# Without file upload middleware, ctx.files is empty, so this should reject
jsonpath "$.tag" == "Rejected"
jsonpath "$.reason" contains "Attachment file not found"

# Step 3: Download file (REQUIRES FILE DOWNLOAD ROUTES)
# Uncomment when GET /files/:id is available:
#
# GET http://localhost:8080/files/{{file_ref}}
#
# HTTP/1.1 200
# [Asserts]
# header "Content-Type" == "text/plain"
# body contains "Hello World!"
