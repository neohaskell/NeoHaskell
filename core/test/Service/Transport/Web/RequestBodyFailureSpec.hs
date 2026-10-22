{-# OPTIONS_GHC -Wno-deprecations #-}

module Service.Transport.Web.RequestBodyFailureSpec where

import Basics
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.MVar qualified as GhcMVar
import Control.Exception qualified as GhcException
import Data.ByteString qualified as GhcBS
import Data.IORef qualified as GhcIORef
import Data.ByteString.Lazy qualified as GhcLBS
import Bytes qualified
import Map qualified
import Network.HTTP.Types.Method qualified as HTTPMethod
import Network.HTTP.Types.Status qualified as HTTPStatus
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Service.Transport (EndpointHandler)
import Service.Transport qualified as Transport
import Service.Transport.Web qualified as Web
import System.Timeout qualified as GhcTimeout
import Task qualified
import Test
import Text qualified
import Prelude qualified as GhcPrelude


spec :: Spec Unit
spec = do
  describe "Service.Transport.Web command request body failures" do
    it "returns a generic 500 when requestBody throws" \_ -> do
      handlerCalled <- Task.fromIO (GhcIORef.newIORef False)
      let endpoints = requestBodyEndpoints (recordHandlerCall handlerCalled)
      let app = webApplication endpoints
      responseResult <-
        Task.fromIO
          ( GhcException.try @GhcException.SomeException
              (WaiTest.runSession (WaiTest.request throwingBodyRequest) app)
          )

      case responseResult of
        GhcPrelude.Left _ -> fail "Expected a response after request body failure"
        GhcPrelude.Right response -> do
          response.simpleStatus |> HTTPStatus.statusCode |> shouldBe 500
          let responseBody = response.simpleBody |> GhcLBS.toStrict |> Bytes.fromLegacy |> Text.fromBytes
          responseBody |> shouldBe "Internal server error"
          Text.contains "private-body-read-details" responseBody |> shouldBe False

      wasHandlerCalled <- GhcIORef.readIORef handlerCalled |> Task.fromIO
      wasHandlerCalled |> shouldBe False

    it "preserves cancellation while requestBody is blocked" \_ -> do
      bodyReadStarted <- Task.fromIO GhcMVar.newEmptyMVar
      bodyRelease <- Task.fromIO GhcMVar.newEmptyMVar
      outcomeVar <- Task.fromIO GhcMVar.newEmptyMVar
      let request = blockedBodyRequest bodyReadStarted bodyRelease
      let app = webApplication (requestBodyEndpoints neverCalledHandler)
      requestThread <- Task.fromIO do
        GhcConcurrent.forkIO do
          outcome <-
            GhcException.try @GhcException.SomeException
              (WaiTest.runSession (WaiTest.request request) app)
          GhcMVar.putMVar outcomeVar outcome

      bodyReadStart <-
        Task.fromIO
          (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar bodyReadStarted))
      case bodyReadStart of
        GhcPrelude.Nothing -> do
          GhcConcurrent.killThread requestThread |> Task.fromIO
          fail "The route did not start reading the request body"
        GhcPrelude.Just () -> pass

      GhcConcurrent.killThread requestThread |> Task.fromIO
      maybeOutcome <-
        Task.fromIO
          (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar outcomeVar))
      case maybeOutcome of
        GhcPrelude.Nothing -> fail "The canceled request did not finish"
        GhcPrelude.Just (GhcPrelude.Left exception) ->
          case GhcException.fromException exception :: GhcPrelude.Maybe GhcException.AsyncException of
            GhcPrelude.Just GhcException.ThreadKilled -> pass
            GhcPrelude.Just _ -> fail "Expected ThreadKilled from the canceled request"
            GhcPrelude.Nothing -> fail "Expected an asynchronous cancellation exception"
        GhcPrelude.Just (GhcPrelude.Right _) -> fail "Expected cancellation to escape the WAI request"


requestBodyEndpoints :: EndpointHandler -> Transport.Endpoints Web.WebTransport
requestBodyEndpoints handler =
  Transport.Endpoints
    { Transport.transport = Web.server
    , Transport.commandEndpoints = Map.fromArray [("BodyProbe", handler)]
    , Transport.queryEndpoints = Map.empty
    , Transport.commandSchemas = Map.empty
    , Transport.querySchemas = Map.empty
    }


recordHandlerCall :: GhcIORef.IORef Bool -> EndpointHandler
recordHandlerCall wasCalled _requestContext _body _respond =
  GhcIORef.writeIORef wasCalled True |> Task.fromIO


neverCalledHandler :: EndpointHandler
neverCalledHandler _requestContext _body _respond = Task.yield ()


throwingBodyRequest :: Wai.Request
throwingBodyRequest =
  commandRequest
    { Wai.requestBody =
        GhcException.throwIO (GhcException.ErrorCall "private-body-read-details")
    }


blockedBodyRequest :: GhcMVar.MVar () -> GhcMVar.MVar GhcBS.ByteString -> Wai.Request
blockedBodyRequest bodyReadStarted bodyRelease =
  commandRequest
    { Wai.requestBody = do
        GhcMVar.putMVar bodyReadStarted ()
        GhcMVar.takeMVar bodyRelease
    }


commandRequest :: Wai.Request
commandRequest =
  Wai.defaultRequest
    { Wai.requestMethod = HTTPMethod.methodPost
    , Wai.rawPathInfo = "/commands/body-probe"
    , Wai.pathInfo = ["commands", "body-probe"]
    }


webApplication :: Transport.Endpoints Web.WebTransport -> Wai.Application
webApplication endpoints request respond = do
  let runnable = Transport.assembleTransport endpoints
  runnable request (\response -> Task.fromIO (respond response))
    |> Task.runOrPanic
