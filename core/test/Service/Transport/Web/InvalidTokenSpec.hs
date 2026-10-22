module Service.Transport.Web.InvalidTokenSpec where

import Basics
import Bytes qualified
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.MVar qualified as GhcMVar
import Control.Exception qualified as GhcException
import Data.Aeson qualified as GhcAeson
import Data.ByteString.Base64.URL qualified as GhcBase64Url
import Data.ByteString.Lazy qualified as GhcLBS
import Data.Text.Encoding qualified as GhcTextEncoding
import Auth.Config (AuthConfig (..))
import Auth.Jwks (JwksManager (..))
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Method qualified as HTTPMethod
import Network.HTTP.Types.Status qualified as HTTPStatus
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test qualified as WaiTest
import Prelude qualified as GhcPrelude
import Service.Transport (EndpointHandler)
import Service.Transport qualified as Transport
import Service.Transport.Web qualified as Web
import System.Timeout qualified as GhcTimeout
import Task qualified
import Test
import Test.Auth.Jwt.Core qualified as Jwt
import Text (Text)
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Transport.Web invalid command token" do
    it "returns 401 without exposing the invalid token" \_ -> do
      keys <- Jwt.testKeys
      manager <- Jwt.createTestManager keys
      let transport =
            Web.server
              { Web.authEnabled =
                  Just
                    Web.AuthEnabled
                      { Web.jwksManager = manager
                      , Web.authConfig = Jwt.testConfig
                      }
              }
      let endpoints = invalidTokenEndpoints transport
      let app = webApplication endpoints
      let request = invalidTokenRequest
      response <-
        WaiTest.runSession
          (WaiTest.srequest (WaiTest.SRequest request ""))
          app
          |> Task.fromIO

      HTTPStatus.statusCode response.simpleStatus |> shouldBe 401
      response.simpleBody
        |> GhcLBS.toStrict
        |> Bytes.fromLegacy
        |> Text.fromBytes
        |> shouldBe "{\"error\":\"Authentication failed\"}"

    it "preserves cancellation during JWKS refresh and reports one command result" \_ -> do
      keys <- Jwt.testKeys
      manager <- Jwt.createTestManager keys
      refreshStarted <- Task.fromIO GhcMVar.newEmptyMVar
      refreshRelease <- Task.fromIO GhcMVar.newEmptyMVar
      requestOutcome <- Task.fromIO GhcMVar.newEmptyMVar

      authCancellation <- Task.fromIO
        ( Warp.testWithApplication
            (GhcPrelude.pure (blockedJwksApplication refreshStarted refreshRelease))
            ( \port -> do
                let localJwksUrl = [fmt|http://127.0.0.1:#{port}/jwks|]
                let managerWithLocalJwks =
                      manager
                        { config =
                            manager.config
                              { jwksUri = localJwksUrl
                              , missingKidCooldownSeconds = 0
                              }
                        }
                let transport =
                      Web.server
                        { Web.authEnabled =
                            Just
                              Web.AuthEnabled
                                { Web.jwksManager = managerWithLocalJwks
                                , Web.authConfig = Jwt.testConfig
                                }
                        }
                let app = webApplication (invalidTokenEndpoints transport)
                let request = requestWithUnknownKid
                requestThread <-
                  GhcConcurrent.forkIO do
                    outcome <-
                      GhcException.try @GhcException.SomeException
                        (WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request "")) app)
                    GhcMVar.putMVar requestOutcome outcome
                let cleanup = do
                      GhcConcurrent.killThread requestThread
                      _ <- GhcMVar.tryPutMVar refreshRelease ()
                      GhcPrelude.pure ()
                GhcException.finally
                  ( do
                      refreshStart <- GhcTimeout.timeout 5000000 (GhcMVar.takeMVar refreshStarted)
                      case refreshStart of
                        GhcPrelude.Nothing -> GhcPrelude.pure (GhcPrelude.Left "Authentication did not reach the controlled JWKS refresh")
                        GhcPrelude.Just () -> do
                          GhcConcurrent.killThread requestThread
                          maybeOutcome <- GhcTimeout.timeout 5000000 (GhcMVar.takeMVar requestOutcome)
                          case maybeOutcome of
                            GhcPrelude.Nothing -> GhcPrelude.pure (GhcPrelude.Left "Canceled authentication request did not finish")
                            GhcPrelude.Just (GhcPrelude.Left exception) ->
                              case GhcException.fromException exception :: GhcPrelude.Maybe GhcException.AsyncException of
                                GhcPrelude.Just GhcException.ThreadKilled -> GhcPrelude.pure (GhcPrelude.Right ())
                                GhcPrelude.Just _ -> GhcPrelude.pure (GhcPrelude.Left "Expected ThreadKilled from the canceled authentication request")
                                GhcPrelude.Nothing -> GhcPrelude.pure (GhcPrelude.Left "Expected authentication cancellation to escape the WAI request")
                            GhcPrelude.Just (GhcPrelude.Right _) -> GhcPrelude.pure (GhcPrelude.Left "Expected cancellation to escape the WAI request")
                  )
                  cleanup
            )
        )

      case authCancellation of
        GhcPrelude.Left errorMessage -> fail (Text.fromLinkedList errorMessage)
        GhcPrelude.Right () -> pass



invalidTokenEndpoints :: Web.WebTransport -> Transport.Endpoints Web.WebTransport
invalidTokenEndpoints transport =
  Transport.Endpoints
    { Transport.transport = transport
    , Transport.commandEndpoints = Map.fromArray [("AuthProbe", rejectInvalidToken)]
    , Transport.queryEndpoints = Map.empty
    , Transport.commandSchemas = Map.empty
    , Transport.querySchemas = Map.empty
    }


rejectInvalidToken :: EndpointHandler
rejectInvalidToken _requestContext _body _respond =
  Task.throw "invalid token reached command handler"


webApplication :: Transport.Endpoints Web.WebTransport -> Wai.Application
webApplication endpoints request respond = do
  let runnable = Transport.assembleTransport endpoints
  runnable request (\response -> Task.fromIO (respond response))
    |> Task.runOrPanic


invalidTokenRequest :: Wai.Request
invalidTokenRequest =
  Wai.defaultRequest
    { Wai.requestMethod = HTTPMethod.methodPost
    , Wai.rawPathInfo = "/commands/auth-probe"
    , Wai.pathInfo = ["commands", "auth-probe"]
    , Wai.requestHeaders =
        [ ( HTTP.hAuthorization
          , Text.concat ["Bearer ", Jwt.makeMalformedToken]
              |> Text.toBytes
              |> Bytes.unwrap
          )
        ]
    }


requestWithUnknownKid :: Wai.Request
requestWithUnknownKid =
  invalidTokenRequest
    { Wai.requestHeaders =
        [ ( HTTP.hAuthorization
          , [fmt|Bearer #{tokenWithUnknownKid}|]
              |> Text.toBytes
              |> Bytes.unwrap
          )
        ]
    }


tokenWithUnknownKid :: Text
tokenWithUnknownKid = do
  let header =
        GhcAeson.object
          [ "alg" GhcAeson..= GhcAeson.String "ES256"
          , "kid" GhcAeson..= GhcAeson.String "missing-key-for-refresh"
          , "typ" GhcAeson..= GhcAeson.String "JWT"
          ]
  let encodedHeader =
        GhcAeson.encode header
          |> GhcLBS.toStrict
          |> GhcBase64Url.encodeUnpadded
          |> GhcTextEncoding.decodeUtf8
  [fmt|#{encodedHeader}.e30.signature|]


blockedJwksApplication :: GhcMVar.MVar () -> GhcMVar.MVar () -> Wai.Application
blockedJwksApplication refreshStarted refreshRelease _request respond = do
  GhcMVar.putMVar refreshStarted ()
  GhcMVar.takeMVar refreshRelease
  respond (Wai.responseLBS HTTPStatus.status200 [] "{\"keys\":[]}")
