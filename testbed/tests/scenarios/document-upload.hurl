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
file: file,project-proposal.pdf; application/pdf

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
# NOTE: Automatic FileRef resolution is not yet implemented (see GitHub issue).
# For now, commands receive an empty ctx.files and must handle missing files.
# This test verifies the current behavior - rejection when file not in context.
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "770e8400-e29b-41d4-a716-446655440002",
  "title": "Q1 Project Proposal",
  "attachment": "{{proposal_file_ref}}"
}

HTTP 400
[Asserts]
# Currently rejected because automatic FileRef resolution is not implemented
jsonpath "$.tag" == "Rejected"
jsonpath "$.reason" contains "not found"


# =============================================================================
# Step 4: Verify file is still downloadable (upload succeeded, command rejected)
# =============================================================================
GET http://localhost:8080/files/{{proposal_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "application/pdf"


# =============================================================================
# Step 5: Creating document with non-existent FileRef should also fail
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
