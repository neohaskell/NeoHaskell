module Service.Api.WebApi (
  WebApi (..),
  server,
) where

import Basics
import Bytes qualified
import Console qualified
import GHC.TypeLits qualified as GHC
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Record (KnownHash (..))
import Record qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler, ApiEndpoints (..))
import Service.Command.Core (Command, NameOf)
import Service.CommandHandler.TH (deriveKnownHash)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


data WebApi = WebApi
  { port :: Int
  }


type instance NameOf WebApi = "WebApi"


deriveKnownHash "WebApi"


server :: WebApi
server =
  WebApi
    { port = 8080
    }


instance ApiBuilder WebApi where
  type Request WebApi = Wai.Request
  type Response WebApi = Wai.Response
  type
    RunnableApi WebApi =
      Wai.Request ->
      (Wai.Response -> Task Text Wai.ResponseReceived) ->
      Task Text Wai.ResponseReceived


  assembleApi ::
    ApiEndpoints WebApi ->
    Wai.Request ->
    (Wai.Response -> Task Text Wai.ResponseReceived) ->
    Task Text Wai.ResponseReceived
  assembleApi endpoints request respond = do
    -- Helper function for 404 responses
    let notFound message = do
          let response404 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status404 [(HTTP.hContentType, "text/plain")]
          respond response404

    -- Parse the request path to check if it matches /commands/<name>
    case Wai.pathInfo request of
      ["commands", commandName] -> do
        -- Look up the command handler in the endpoints map
        case Map.get commandName endpoints.commandEndpoints of
          Maybe.Just handler -> do
            -- Read the request body
            requestBody <- Wai.strictRequestBody request |> Task.fromIO
            let bodyBytes = requestBody |> Bytes.fromLazyLegacy

            -- We need to capture the ResponseReceived from the handler's callback
            -- Since handler returns Unit, we'll use andThen to chain the result
            handler
              bodyBytes
              ( \responseBytes -> do
                  -- Create a WAI response from the handler's response
                  let responseBody =
                        responseBytes
                          |> Bytes.toLazyLegacy
                          |> Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")]
                  _ <- respond responseBody
                  Task.yield ()
              )
              |> Task.andThen
                ( \_ ->
                    -- After the handler completes, we need to return a ResponseReceived
                    -- But the actual ResponseReceived was already sent via respond
                    -- This is a fundamental mismatch - let me rethink...
                    panic "Need to restructure how ResponseReceived flows through"
                )
          Maybe.Nothing ->
            notFound [fmt|Command not found: {commandName}|]
      _ ->
        notFound "Not found"


  runApi :: WebApi -> RunnableApi WebApi -> Task Text Unit
  runApi api runnableApi = do
    -- RunnableApi WebApi is: Wai.Request -> (Wai.Response -> Task Text ResponseReceived) -> Task Text ResponseReceived
    -- We need to convert this to a WAI Application: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    let waiApp :: Wai.Application
        waiApp request respond = do
          -- Create a Task-based response handler that wraps the IO respond function
          let taskRespond response = Task.fromIO (respond response)

          -- Run the Task-based application and return the ResponseReceived
          runnableApi request taskRespond
            |> Task.runOrPanic

    -- Start the Warp server on the specified port
    Console.print [fmt|Starting WebApi server on port {api.port}|]
    Warp.run api.port waiApp |> Task.fromIO


  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebApi ->
    Record.Proxy command ->
    ApiEndpointHandler
  buildCommandHandler api _ body respond = do
    let port = api.port
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    Console.print [fmt|Running #{n} on port #{port}|]
    Console.print (body |> Text.fromBytes)
    respond (Text.toBytes n)
