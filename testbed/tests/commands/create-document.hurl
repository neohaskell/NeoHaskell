# Create Document Command Test
# Tests the create-document endpoint that references file uploads
#
# Note: Full file upload e2e testing requires WebTransport file routes
# (POST /files/upload) which are not yet wired. This test verifies the
# command endpoint exists and properly rejects invalid file references.

# Test 1: CreateDocument endpoint exists and returns proper error for missing file
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440000",
  "title": "Test Document",
  "attachment": "file_nonexistent123"
}

HTTP/1.1 400
[Asserts]
# Command should be rejected because the file ref is not in context
# (no file upload middleware wired yet, so ctx.files is always empty)
jsonpath "$.tag" == "Rejected"
jsonpath "$.reason" contains "Attachment file not found"
