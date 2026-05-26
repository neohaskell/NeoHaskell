# E2E test: readiness endpoint
# Verifies that /health returns 200 immediately and /ready returns 200 after query rebuild completes.
#
# This test exercises the async query rebuild feature:
# - The HTTP server binds before query rebuild completes
# - /health returns 200 right away (liveness)
# - /ready returns 200 after subscriber.ready is True

# Health check must respond immediately
GET http://localhost:8080/health
HTTP/1.1 200

# Readiness check must respond after rebuild completes
# Since the testbed has queries (CartSummary, StockLevel),
# the /ready endpoint should eventually return 200 after rebuild finishes.
# We retry because rebuild runs asynchronously.
GET http://localhost:8080/ready
[Options]
retry: 20
retry-interval: 500
HTTP/1.1 200
