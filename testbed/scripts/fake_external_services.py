#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import json
import base64
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlencode, urlsplit


HOST = "127.0.0.1"
PORT = 8181
ISSUER = "http://127.0.0.1:8181/oidc"
CLIENT_ID = "testbed-client"
CLIENT_SECRET = "testbed-fake-client-secret"
REDIRECT_URI = "http://localhost:8081/callback/test-provider"
AUTHORIZATION_CODE = "fixture-authorization-code"
FIXTURE_JWT = ".".join(
    (
        "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RiZWQtcnMyNTYifQ",
        "eyJpc3MiOiJodHRwOi8vMTI3LjAuMC4xOjgxODEvb2lkYyIsInN1YiI6ImZpeHR1cmUtdXNlciIsImlhdCI6MTc2NzIyNTYwMCwiZXhwIjo0MTAyNDQ0ODAwfQ",
        "H08QidefVv5zqF69ZvJUzNsTyJuhzvh6ATmQMyYEtSvChwnn_s-GDvb_z7Sl20muQGTNHI7PEil2s7VED9zaG-KnDau02Yoy35t-AvQT7YNDaQZn5qj321Kr-CSXGrkA1SXeyGLcwzXwH4RMz8UuQt9gzNYNQaSxW0C1XNNmP5ccPxEMuW-1mf34bEl-GzHLjPgqZrMcQTjl59Tf0_GtyCpxn7VEm5XNIAVmKef-Y5-mehSJKtJTrdvKpjbVBf4fYQ90VpWhCJxkGuMIga3JIgH86NJ5ZQ7cO3BOasp4IdlRA1RE9Jrp5HJ6d41Bt0w6L5scmsnRd549khbitIRDng",
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
        }
    ]
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
    }


def decode_form(body: bytes) -> dict[str, str]:
    return {key: values[0] for key, values in parse_qs(body.decode("utf-8")).items() if values}


def pkce_challenge(verifier: str) -> str:
    digest = hashlib.sha256(verifier.encode("ascii")).digest()
    return base64.urlsafe_b64encode(digest).rstrip(b"=").decode("ascii")


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

    def read_json(self) -> dict | None:
        length = int(self.headers.get("Content-Length", "0"))
        if length <= 0 or length > 16384:
            return None
        try:
            value = json.loads(self.rfile.read(length))
        except (json.JSONDecodeError, UnicodeDecodeError):
            return None
        return value if isinstance(value, dict) else None

    def do_GET(self) -> None:
        path = urlsplit(self.path).path
        if path == "/health":
            self.send_json({"status": "ok", "fixture": "neohaskell-testbed"})
        elif path == "/fixture/jwt":
            self.send_json({"accessToken": FIXTURE_JWT})
        elif path == "/oidc/.well-known/openid-configuration":
            self.send_json(
                {
                    "issuer": ISSUER,
                    "jwks_uri": f"{ISSUER}/jwks",
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
        if not isinstance(effect_id, str) or not effect_id or idempotency_key != effect_id:
            self.send_json({"error": "invalid effect identity"}, 400)
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
            "outcome": "rejected" if payload.get("description") == "force-failure" else "accepted",
            "httpStatus": 422 if payload.get("description") == "force-failure" else 201,
        }
        row["requestCount"] += 1
        effects[effect_id] = row
        self.send_json(
            {"externalId": external_id},
            row["httpStatus"],
        )


if __name__ == "__main__":
    FixtureServer().serve_forever()
