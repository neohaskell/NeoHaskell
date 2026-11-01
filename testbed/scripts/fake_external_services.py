#!/usr/bin/env python3
from __future__ import annotations

import base64
import hashlib
import hmac
import json
import os
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlencode, urlsplit


HOST = "127.0.0.1"
PORT = int(os.environ.get("TESTBED_FIXTURE_PORT", "8181"))
ISSUER = "http://127.0.0.1:8181/oidc"
JWKS_URI = f"http://127.0.0.1:{PORT}/oidc/jwks"
CLIENT_ID = "testbed-client"
CLIENT_SECRET = "testbed-fake-client-secret"
REDIRECT_URI = f"http://localhost:{os.environ.get('TESTBED_OAUTH_PORT', '8081')}/callback/test-provider"
AUTHORIZATION_CODE = "fixture-authorization-code"
S3_BUCKET = os.environ.get("S3_BUCKET", "testbed-s3-bucket")
S3_REGION = os.environ.get("S3_REGION", "us-east-1")
S3_ACCESS_KEY_ID = os.environ.get("S3_ACCESS_KEY_ID", "testbed-s3-access-key")
S3_SECRET_ACCESS_KEY = os.environ.get("S3_SECRET_ACCESS_KEY", "testbed-s3-secret-key")
FIXTURE_JWT = ".".join(
    (
        "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RiZWQtcnMyNTYifQ",
        "eyJpc3MiOiJodHRwOi8vMTI3LjAuMC4xOjgxODEvb2lkYyIsInN1YiI6ImZpeHR1cmUtdXNlciIsImlhdCI6MTc2NzIyNTYwMCwiZXhwIjo0MTAyNDQ0ODAwfQ",
        "H08QidefVv5zqF69ZvJUzNsTyJuhzvh6ATmQMyYEtSvChwnn_s-GDvb_z7Sl20muQGTNHI7PEil2s7VED9zaG-KnDau02Yoy35t-AvQT7YNDaQZn5qj321Kr-CSXGrkA1SXeyGLcwzXwH4RMz8UuQt9gzNYNQaSxW0C1XNNmP5ccPxEMuW-1mf34bEl-GzHLjPgqZrMcQTjl59Tf0_GtyCpxn7VEm5XNIAVmKef-Y5-mehSJKtJTrdvKpjbVBf4fYQ90VpWhCJxkGuMIga3JIgH86NJ5ZQ7cO3BOasp4IdlRA1RE9Jrp5HJ6d41Bt0w6L5scmsnRd549khbitIRDng",
    )
)
OTHER_FIXTURE_JWT = ".".join(
    (
        "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RiZWQtb3RoZXItcnMyNTYifQ",
        "eyJpc3MiOiJodHRwOi8vMTI3LjAuMC4xOjgxODEvb2lkYyIsInN1YiI6ImZpeHR1cmUtb3RoZXItdXNlciIsImlhdCI6MTc2NzIyNTYwMCwiZXhwIjo0MTAyNDQ0ODAwfQ",
        "X7R-b5LTTfSa6xh9k5N8YzTFX9epqCpUe82Jiufz7CYUFx19JTMHh_AQhtEBlKyh8HdrMmqydb7sbk7BiavkLYgMotey9OxUKTqGmtWvdBSSy8r1l_Cg3OHYHXaGvkhMkQHOms5RIp9JVrI84nCqj_wVCzbjG47adZaipYD7iNqknxDpMXHP2QSArY9EDMQY_D8ONWdzKRjqmD9pA2e-YRm9eI7j5Y0C1phFTn7MgxhBdMxcH-wUxWjC9nSfzR4uxGqhvKkueoVl0M5Mt25Nru7XHW8u8iyNSn2LFcoOR_bmTQZGlPlHNXiyj1vGx7tsd7nCwKnhAdpq2RIKozFJFg",
    )
)
JWKS = {
    "keys": [
        {
            "kty": "RSA",
            "use": "sig",
            "alg": "RS256",
            "kid": "testbed-rs256",
            "n": "opV9TGzqrFh0a7YMCPoTD2gE7AqbTMt422iCEHggApv2wGYZZNVDACdaOKdlE75h7zfMOS7M4Z1r1eEIUsGMlYrm9aQLj5Y4OFfKvyUusholiL6-Ree6BDFoeKyqOwXHRv9Fz5KeGeSKb3j9lzeL6fudss-kJhW8WB35l8W2eFfnZDDHtong8xqRh_mz2LacxktcwF4rBaxjw0xSbzx2iHJ7SiNzQu55rxRFgFq32Tpfr7VIrhKgHPwJ2-sS85w-69qJLTcLKjhbe9PlPYpnWxtfv5tuW_wgEMRdedZ1JWpcCXiyvg9h5C6btIqQIlLPNhRBFgFQAuDTY4sgGC1QQQ",
            "e": "AQAB",
        },
        {
            "kty": "RSA",
            "use": "sig",
            "alg": "RS256",
            "kid": "testbed-other-rs256",
            "n": "3qlmtkme0nkTEOFEjib-F0z1bx1bM6BWCR7GZHTvtc3TXGeyTgLJiLKF34BwadbJ86rUAvAhxrEHf5-xmLX9t3INDVCYNNEyILcnpmImFo6WF7dGa4ONiVFoz-wBdRtxK6822qff1ggF5LTPRfhW6Wd-xy09VCzDmfiAdnQZZ1OkKixe6nCEuILI1NZTaQ6tO1kL0GOoZMfaXpSTTwpAqK9iJc80y13IBSQBdJ7v-KrctgINnmCsCngy6btCX0HEaO71QmzjbmT4BLkfzCUZ_rlvbbZpkV4GfZsZWNU-TFjXjAu7OnqVgKZzHFTWEZAvvHek64X5MOSqeZwIop0-fw",
            "e": "AQAB",
        },
    ]
}


def empty_s3_state() -> dict:
    return {
        "objects": {},
        "putCount": 0,
        "getCount": 0,
        "headCount": 0,
        "deleteCount": 0,
        "rejectedCount": 0,
    }


def empty_state() -> dict:
    return {
        "effects": {},
        "authorizationRequests": 0,
        "authorizationValid": False,
        "tokenRequests": 0,
        "authorizationCodeVerified": False,
        "clientVerified": False,
        "redirectUriVerified": False,
        "pkceVerified": False,
        "tokenExchangeSucceeded": False,
        "pkceChallenge": None,
        "s3": empty_s3_state(),
    }


def decode_form(body: bytes) -> dict[str, str]:
    return {key: values[0] for key, values in parse_qs(body.decode("utf-8")).items() if values}


def pkce_challenge(verifier: str) -> str:
    digest = hashlib.sha256(verifier.encode("ascii")).digest()
    return base64.urlsafe_b64encode(digest).rstrip(b"=").decode("ascii")


def hmac_sha256(key: bytes, message: bytes) -> bytes:
    return hmac.new(key, message, hashlib.sha256).digest()


def verify_s3_request(method: str, raw_path: str, headers: dict[str, str], body: bytes) -> bool:
    authorization = headers.get("authorization", "")
    if not authorization.startswith("AWS4-HMAC-SHA256 "):
        return False
    attributes = {}
    for item in authorization.removeprefix("AWS4-HMAC-SHA256 ").split(", "):
        if "=" not in item:
            return False
        name, value = item.split("=", 1)
        attributes[name] = value
    credential = attributes.get("Credential", "")
    signed_headers_raw = attributes.get("SignedHeaders", "")
    received_signature = attributes.get("Signature", "")
    parts = credential.split("/")
    if len(parts) != 5:
        return False
    access_key, date_stamp, region, service, terminator = parts
    if (
        access_key != S3_ACCESS_KEY_ID
        or region != S3_REGION
        or service != "s3"
        or terminator != "aws4_request"
        or not date_stamp
    ):
        return False
    amz_date = headers.get("x-amz-date", "")
    payload_hash = hashlib.sha256(body).hexdigest()
    if not amz_date.startswith(date_stamp) or headers.get("x-amz-content-sha256") != payload_hash:
        return False
    signed_names = signed_headers_raw.split(";")
    for required in ("host", "x-amz-content-sha256", "x-amz-date"):
        if required not in signed_names or not headers.get(required):
            return False
    canonical_headers = "".join(
        f"{name}:{' '.join(headers.get(name, '').split())}\n" for name in signed_names
    )
    canonical_request = "\n".join(
        (method, raw_path, "", canonical_headers, signed_headers_raw, payload_hash)
    )
    request_hash = hashlib.sha256(canonical_request.encode("utf-8")).hexdigest()
    scope = f"{date_stamp}/{region}/s3/aws4_request"
    string_to_sign = "\n".join(("AWS4-HMAC-SHA256", amz_date, scope, request_hash))
    date_key = hmac_sha256(("AWS4" + S3_SECRET_ACCESS_KEY).encode("utf-8"), date_stamp.encode("utf-8"))
    region_key = hmac_sha256(date_key, region.encode("utf-8"))
    service_key = hmac_sha256(region_key, b"s3")
    signing_key = hmac_sha256(service_key, b"aws4_request")
    expected_signature = hmac.new(
        signing_key, string_to_sign.encode("utf-8"), hashlib.sha256
    ).hexdigest()
    return hmac.compare_digest(received_signature, expected_signature)


class FixtureServer(HTTPServer):
    def __init__(self) -> None:
        super().__init__((HOST, PORT), FixtureHandler)
        self.fixture_state = empty_state()


class FixtureHandler(BaseHTTPRequestHandler):
    server: FixtureServer
    protocol_version = "HTTP/1.1"

    def end_headers(self) -> None:
        self.send_header("Connection", "close")
        self.close_connection = True
        super().end_headers()

    def log_message(self, _format: str, *args: object) -> None:
        return

    def send_json(self, payload: dict, status: int = 200) -> None:
        encoded = json.dumps(payload, separators=(",", ":")).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

    def send_s3_xml_error(self, code: str, status: int) -> None:
        encoded = f"<Error><Code>{code}</Code></Error>".encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/xml")
        self.send_header("Content-Length", str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

    def read_body(self) -> bytes:
        length = int(self.headers.get("Content-Length", "0"))
        if length <= 0:
            return b""
        return self.rfile.read(length)

    def read_json(self) -> dict | None:
        length = int(self.headers.get("Content-Length", "0"))
        if length <= 0 or length > 16384:
            return None
        try:
            value = json.loads(self.rfile.read(length))
        except (json.JSONDecodeError, UnicodeDecodeError):
            return None
        return value if isinstance(value, dict) else None

    def lowercase_headers(self) -> dict[str, str]:
        return {key.lower(): value for key, value in self.headers.items()}

    def is_s3_bucket_path(self, path: str) -> bool:
        return path.startswith(f"/{S3_BUCKET}/")

    def handle_s3(self, method: str) -> None:
        raw_path = urlsplit(self.path).path
        body = self.read_body()
        s3_state = self.server.fixture_state["s3"]
        if not verify_s3_request(method, raw_path, self.lowercase_headers(), body):
            s3_state["rejectedCount"] += 1
            if method == "HEAD":
                self.send_response(403)
                self.send_header("Content-Length", "0")
                self.end_headers()
            else:
                self.send_s3_xml_error("AccessDenied", 403)
            return
        objects = s3_state["objects"]
        if method == "PUT":
            objects[raw_path] = body
            s3_state["putCount"] += 1
            etag = f'"{hashlib.sha256(body).hexdigest()[:16]}-{len(body)}"'
            self.send_response(200)
            self.send_header("ETag", etag)
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif method == "GET":
            s3_state["getCount"] += 1
            stored = objects.get(raw_path)
            if stored is None:
                self.send_s3_xml_error("NoSuchKey", 404)
                return
            etag = f'"{hashlib.sha256(stored).hexdigest()[:16]}-{len(stored)}"'
            self.send_response(200)
            self.send_header("ETag", etag)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Content-Length", str(len(stored)))
            self.end_headers()
            self.wfile.write(stored)
        elif method == "HEAD":
            s3_state["headCount"] += 1
            stored = objects.get(raw_path)
            if stored is None:
                self.send_response(404)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            etag = f'"{hashlib.sha256(stored).hexdigest()[:16]}-{len(stored)}"'
            self.send_response(200)
            self.send_header("ETag", etag)
            self.send_header("Content-Length", str(len(stored)))
            self.end_headers()
        elif method == "DELETE":
            s3_state["deleteCount"] += 1
            objects.pop(raw_path, None)
            self.send_response(204)
            self.send_header("Content-Length", "0")
            self.end_headers()

    def do_PUT(self) -> None:
        path = urlsplit(self.path).path
        if self.is_s3_bucket_path(path):
            self.handle_s3("PUT")
        else:
            self.send_json({"error": "not found"}, 404)

    def do_HEAD(self) -> None:
        path = urlsplit(self.path).path
        if self.is_s3_bucket_path(path):
            self.handle_s3("HEAD")
        else:
            self.send_response(404)
            self.send_header("Content-Length", "0")
            self.end_headers()

    def do_DELETE(self) -> None:
        path = urlsplit(self.path).path
        if self.is_s3_bucket_path(path):
            self.handle_s3("DELETE")
        else:
            self.send_json({"error": "not found"}, 404)

    def do_GET(self) -> None:
        path = urlsplit(self.path).path
        if self.is_s3_bucket_path(path):
            self.handle_s3("GET")
        elif path == "/health":
            self.send_json({"status": "ok", "fixture": "neohaskell-testbed"})
        elif path == "/s3/observations":
            s3_state = self.server.fixture_state["s3"]
            self.send_json(
                {
                    "putCount": s3_state["putCount"],
                    "getCount": s3_state["getCount"],
                    "headCount": s3_state["headCount"],
                    "deleteCount": s3_state["deleteCount"],
                    "rejectedCount": s3_state["rejectedCount"],
                    "objectCount": len(s3_state["objects"]),
                }
            )
        elif path == "/fixture/jwt":
            self.send_json({"accessToken": FIXTURE_JWT})
        elif path == "/fixture/other-jwt":
            self.send_json({"accessToken": OTHER_FIXTURE_JWT})
        elif path == "/oidc/.well-known/openid-configuration":
            self.send_json(
                {
                    "issuer": ISSUER,
                    "jwks_uri": JWKS_URI,
                }
            )
        elif path == "/oidc/jwks":
            self.send_json(JWKS)
        elif path == "/oauth/authorize":
            self.handle_authorization()
        elif path == "/oauth/observations":
            self.send_json(
                {
                    "authorizationRequests": self.server.fixture_state["authorizationRequests"],
                    "authorizationValid": self.server.fixture_state["authorizationValid"],
                    "tokenRequests": self.server.fixture_state["tokenRequests"],
                    "authorizationCodeVerified": self.server.fixture_state["authorizationCodeVerified"],
                    "clientVerified": self.server.fixture_state["clientVerified"],
                    "redirectUriVerified": self.server.fixture_state["redirectUriVerified"],
                    "pkceVerified": self.server.fixture_state["pkceVerified"],
                    "tokenExchangeSucceeded": self.server.fixture_state["tokenExchangeSucceeded"],
                }
            )
        elif path.startswith("/external/effects/"):
            effect_id = path.removeprefix("/external/effects/")
            effect = self.server.fixture_state["effects"].get(effect_id)
            if effect is None:
                self.send_json({"error": "effect not found"}, 404)
            else:
                self.send_json(effect)
        else:
            self.send_json({"error": "not found"}, 404)

    def do_POST(self) -> None:
        path = urlsplit(self.path).path
        if path == "/fixture/reset":
            self.server.fixture_state = empty_state()
            self.send_json({"status": "reset"})
        elif path == "/fixture/reset/s3":
            self.server.fixture_state["s3"] = empty_s3_state()
            self.send_json({"status": "reset"})
        elif path == "/fixture/reset/external-effects":
            self.server.fixture_state["effects"] = {}
            self.send_json({"status": "reset"})
        elif path == "/fixture/reset/oauth2":
            fresh_state = empty_state()
            for key in (
                "authorizationRequests",
                "authorizationValid",
                "tokenRequests",
                "authorizationCodeVerified",
                "clientVerified",
                "redirectUriVerified",
                "pkceVerified",
                "tokenExchangeSucceeded",
                "pkceChallenge",
            ):
                self.server.fixture_state[key] = fresh_state[key]
            self.send_json({"status": "reset"})
        elif path == "/external/effects":
            self.handle_external_effect()
        elif path == "/oauth/token":
            self.handle_token_exchange()
        else:
            self.send_json({"error": "not found"}, 404)

    def handle_authorization(self) -> None:
        params = {key: values[0] for key, values in parse_qs(urlsplit(self.path).query).items() if values}
        state = self.server.fixture_state
        state["authorizationRequests"] += 1
        checks = {
            "client_id": params.get("client_id") == CLIENT_ID,
            "response_type": params.get("response_type") == "code",
            "redirect_uri": params.get("redirect_uri") == REDIRECT_URI,
            "code_challenge": bool(params.get("code_challenge")),
            "code_challenge_method": params.get("code_challenge_method") == "S256",
            "state": bool(params.get("state")),
        }
        state["authorizationValid"] = all(checks.values())
        if not state["authorizationValid"]:
            self.send_json({"error": "invalid authorization request", "checks": checks}, 400)
            return
        state["pkceChallenge"] = params["code_challenge"]
        callback = REDIRECT_URI + "?" + urlencode({"code": AUTHORIZATION_CODE, "state": params["state"]})
        self.send_response(302)
        self.send_header("Location", callback)
        self.send_header("Content-Length", "0")
        self.end_headers()

    def handle_token_exchange(self) -> None:
        length = int(self.headers.get("Content-Length", "0"))
        if length <= 0 or length > 16384:
            self.send_json({"error": "invalid_request"}, 400)
            return
        params = decode_form(self.rfile.read(length))
        state = self.server.fixture_state
        state["tokenRequests"] += 1
        state["authorizationCodeVerified"] = params.get("code") == AUTHORIZATION_CODE
        state["clientVerified"] = (
            params.get("client_id") == CLIENT_ID
            and params.get("client_secret") == CLIENT_SECRET
        )
        state["redirectUriVerified"] = params.get("redirect_uri") == REDIRECT_URI
        verifier = params.get("code_verifier", "")
        state["pkceVerified"] = bool(state["pkceChallenge"]) and pkce_challenge(verifier) == state["pkceChallenge"]
        state["tokenExchangeSucceeded"] = all(
            [
                state["authorizationCodeVerified"],
                state["clientVerified"],
                state["redirectUriVerified"],
                state["pkceVerified"],
                params.get("grant_type") == "authorization_code",
            ]
        )
        if not state["tokenExchangeSucceeded"]:
            self.send_json({"error": "invalid_grant"}, 400)
            return
        self.send_json(
            {
                "access_token": "fixture-access-token",
                "refresh_token": "fixture-refresh-token",
                "expires_in": 3600,
                "token_type": "Bearer",
            }
        )

    def handle_external_effect(self) -> None:
        payload = self.read_json()
        if payload is None:
            self.send_json({"error": "invalid JSON body"}, 400)
            return
        effect_id = payload.get("effectId")
        idempotency_key = self.headers.get("Idempotency-Key")
        handler_name = self.headers.get("X-Event-Model-Handler")
        if not isinstance(effect_id, str) or not effect_id or idempotency_key != effect_id:
            self.send_json({"error": "invalid effect identity"}, 400)
            return
        if handler_name not in ("primary", "audit"):
            self.send_json({"error": "unknown effect handler"}, 400)
            return
        effects = self.server.fixture_state["effects"]
        external_id = f"test-effect-{effect_id}"
        existing = effects.get(effect_id)
        if existing is not None and existing["payload"] != payload:
            self.send_json({"error": "idempotency key reused with different payload"}, 409)
            return
        row = existing or {
            "externalId": external_id,
            "payload": payload,
            "idempotencyKey": idempotency_key,
            "requestCount": 0,
            "handlerCounts": {"primary": 0, "audit": 0},
            "outcome": "rejected" if payload.get("description") == "force-failure" else "accepted",
            "httpStatus": 422 if payload.get("description") == "force-failure" else 201,
        }
        row["requestCount"] += 1
        row["handlerCounts"][handler_name] += 1
        effects[effect_id] = row
        self.send_json(
            {"externalId": external_id},
            row["httpStatus"],
        )


if __name__ == "__main__":
    FixtureServer().serve_forever()
