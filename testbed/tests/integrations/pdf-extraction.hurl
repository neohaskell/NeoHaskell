# PDF Text Extraction Integration Tests
#
# These tests verify the PDF extraction integration (Integration.Pdf.ExtractText).
# 
# The integration uses pdftotext and pdfinfo from poppler-utils to extract
# text and metadata from uploaded PDF files.
#
# Prerequisites:
# - poppler-utils installed (pdftotext, pdfinfo available in PATH)
# - testbed running with file upload enabled
#
# NOTE: Full E2E extraction requires FileAccessContext to be wired in
# Service.Application. Until then, only upload tests will pass.

# =============================================================================
# Test 1: PDF file can be uploaded successfully
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test-document.pdf; application/pdf

HTTP 200
[Captures]
pdf_file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "test-document.pdf"
jsonpath "$.contentType" == "application/pdf"
jsonpath "$.sizeBytes" > 0
jsonpath "$.expiresAt" exists


# =============================================================================
# Test 2: Uploaded PDF can be downloaded and verified
# =============================================================================
GET http://localhost:8080/files/{{pdf_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "application/pdf"
header "Content-Disposition" contains "test-document.pdf"


# =============================================================================
# Test 3: Project proposal PDF upload
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,project-proposal.pdf; application/pdf

HTTP 200
[Captures]
proposal_file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.contentType" == "application/pdf"


# =============================================================================
# Test 4: Multiple PDF uploads generate unique FileRefs
# =============================================================================
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test-document.pdf; application/pdf

HTTP 200
[Captures]
pdf_ref_1: jsonpath "$.fileRef"

POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test-document.pdf; application/pdf

HTTP 200
[Captures]
pdf_ref_2: jsonpath "$.fileRef"
[Asserts]
# Each upload should get a unique FileRef
jsonpath "$.fileRef" != "{{pdf_ref_1}}"


# =============================================================================
# Test 5: Create document triggers PDF extraction integration
# =============================================================================
POST http://localhost:8080/commands/create-document
{
  "documentId": "550e8400-e29b-41d4-a716-446655440099",
  "title": "PDF Extraction Test Document",
  "attachment": "{{pdf_file_ref}}"
}

HTTP 200
[Asserts]
jsonpath "$.tag" == "Accepted"


# =============================================================================
# Test 6: Verify extracted text in query results
# (Uses retry to wait for async PDF extraction to complete)
# =============================================================================
GET http://localhost:8080/queries/document-content/550e8400-e29b-41d4-a716-446655440099
[Options]
retry: 10
retry-interval: 500

HTTP 200
[Asserts]
jsonpath "$.extractedText" contains "Hello NeoHaskell PDF Test"
jsonpath "$.pageCount" == 1
