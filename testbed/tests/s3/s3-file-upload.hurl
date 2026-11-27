POST http://localhost:8181/fixture/reset/s3

HTTP 200


GET http://localhost:8181/s3/observations

HTTP 200
[Asserts]
jsonpath "$.putCount" == 0
jsonpath "$.getCount" == 0
jsonpath "$.rejectedCount" == 0
jsonpath "$.objectCount" == 0


PUT http://localhost:8181/testbed-s3-bucket/unsigned-probe
```
unauthorized-payload
```

HTTP 403


GET http://localhost:8181/s3/observations

HTTP 200
[Asserts]
jsonpath "$.rejectedCount" == 1
jsonpath "$.objectCount" == 0


POST http://localhost:8181/fixture/reset/s3

HTTP 200


POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,crlf-test.png; image/png

HTTP 200
[Captures]
s3_file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.fileRef" matches /^file_[a-f0-9-]+$/
jsonpath "$.filename" == "crlf-test.png"
jsonpath "$.contentType" == "image/png"
jsonpath "$.sizeBytes" == 70
jsonpath "$.expiresAt" exists


GET http://localhost:8181/s3/observations

HTTP 200
[Asserts]
jsonpath "$.putCount" == 1
jsonpath "$.objectCount" == 1
jsonpath "$.rejectedCount" == 0


GET http://localhost:8080/files/{{s3_file_ref}}

HTTP 200
[Asserts]
header "Content-Type" == "image/png"
header "Content-Disposition" contains "crlf-test.png"
sha256 == hex,bc09c2590d2502c8ffaf1a3c09aa89df222e03d186a8daa0c7fce6321fb6e928;


GET http://localhost:8181/s3/observations

HTTP 200
[Asserts]
jsonpath "$.putCount" == 1
jsonpath "$.getCount" == 1
jsonpath "$.rejectedCount" == 0


POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,crlf-test.png; image/png

HTTP 200
[Asserts]
jsonpath "$.fileRef" == "{{s3_file_ref}}"


GET http://localhost:8181/s3/observations

HTTP 200
[Asserts]
jsonpath "$.putCount" == 1
jsonpath "$.headCount" >= 1
jsonpath "$.rejectedCount" == 0


GET http://localhost:8080/files/file_nonexistent_00000000-0000-0000-0000-000000000000

HTTP 404
[Asserts]
jsonpath "$.error" contains "not found"
