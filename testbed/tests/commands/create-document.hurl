# Create Document Command Tests
# Tests the create-document endpoint that references file uploads

# =============================================================================
# Test 1: CreateDocument with invalid FileRef returns proper error
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440000",
  "title": "Test Document",
  "attachment": "file_nonexistent_ref_12345"
}

HTTP 400
[Asserts]
jsonpath "$.tag" == "Rejected"
jsonpath "$.reason" contains "not found"


# =============================================================================
# Test 2: CreateDocument with malformed FileRef
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440001",
  "title": "Test Document 2",
  "attachment": "invalid-format"
}

HTTP 400
[Asserts]
jsonpath "$.tag" == "Rejected"


# =============================================================================
# Test 3: CreateDocument with empty attachment field should fail
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440002",
  "title": "Test Document 3",
  "attachment": ""
}

HTTP 400
[Asserts]
jsonpath "$.tag" == "Rejected"


# =============================================================================
# Test 4: CreateDocument with missing required fields returns parse error
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440003",
  "title": "Missing Attachment"
}

HTTP 400
[Asserts]
# Should fail to parse due to missing 'attachment' field
jsonpath "$.tag" == "Failed"
