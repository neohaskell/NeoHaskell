# Document Upload Full E2E Scenario
#
# This scenario tests the complete file upload flow:
# 1. Upload a file -> get FileRef
# 2. Create document referencing the FileRef -> file gets confirmed
# 3. Download the file to verify content
#
# This demonstrates the two-phase upload pattern from ADR-0011.

# =============================================================================
# Step 1: Upload a document file
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,project-proposal.pdf; application/pdf; JVBERi0xLjQKMSAwIG9iago8PAovVHlwZSAvQ2F0YWxvZwo+PgplbmRvYmoKdHJhaWxlcgo8PAovUm9vdCAxIDAgUgo+PgolJUVPRgo=

HTTP 200
[Captures]
proposal_file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "project-proposal.pdf"
jsonpath "$.contentType" == "application/pdf"
jsonpath "$.sizeBytes" > 0
jsonpath "$.expiresAt" exists


# =============================================================================
# Step 2: Verify file can be downloaded (while pending)
# =============================================================================
GET http://localhost:8080/files/{{proposal_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "application/pdf"
header "Content-Disposition" contains "project-proposal.pdf"


# =============================================================================
# Step 3: Create document with the uploaded file
# The framework resolves the FileRef and populates ctx.files before decide runs
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "770e8400-e29b-41d4-a716-446655440002",
  "title": "Q1 Project Proposal",
  "attachment": "{{proposal_file_ref}}"
}

HTTP 200
[Asserts]
jsonpath "$.tag" == "Accepted"
jsonpath "$.entityId" == "770e8400-e29b-41d4-a716-446655440002"


# =============================================================================
# Step 4: Verify file is still downloadable after confirmation
# =============================================================================
GET http://localhost:8080/files/{{proposal_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "application/pdf"


# =============================================================================
# Step 5: Creating document with same file should fail (file already confirmed)
# Or succeed if we allow multiple references - depends on business rules
# For now, just verify the command endpoint works with the same ref
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "880e8400-e29b-41d4-a716-446655440003",
  "title": "Another Document",
  "attachment": "{{proposal_file_ref}}"
}

HTTP 200
[Asserts]
# Second document referencing same file should also work
jsonpath "$.tag" == "Accepted"


# =============================================================================
# Step 6: Creating document with non-existent FileRef should fail
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "990e8400-e29b-41d4-a716-446655440004",
  "title": "Bad Document",
  "attachment": "file_does_not_exist_12345"
}

HTTP 400
[Asserts]
jsonpath "$.tag" == "Rejected"
jsonpath "$.reason" contains "not found"
