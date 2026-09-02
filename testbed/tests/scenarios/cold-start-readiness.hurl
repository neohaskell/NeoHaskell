# Cold-start liveness probe used by testbed/scripts/cold-start-readiness.sh.
# The shell harness owns process timing, event-log seeding, and the simultaneous
# /ready assertion; this request proves the bound process serves the public
# health contract through the same Hurl client users deploy.

GET http://localhost:8080/health

HTTP/1.1 200
Content-Type: application/json

[Asserts]
jsonpath "$.status" == "ok"
