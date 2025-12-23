module Service.Api.WebApi (
  WebApi (..),
  server,
) where

import Basics
import Bytes qualified
import ConcurrentVar qualified
import Console qualified
import GHC.TypeLits qualified as GHC
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Record (KnownHash (..))
import Record qualified
import Result qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler, ApiEndpoints (..))
import Service.Command.Core (Command, NameOf)
import Service.CommandHandler.TH (deriveKnownHash)
import Service.CommandResponse (CommandResponse)
import Service.CommandResponse qualified as CommandResponse
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
                  |> Wai.responseLBS HTTP.status404 [(HTTP.hContentType, "application/json")]
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

            respondVar <- ConcurrentVar.new
            -- We need to capture the ResponseReceived from the handler's callback
            -- Since handler returns Unit, we'll use andThen to chain the result
            handler
              bodyBytes
              ( \responseBytes -> do
                  -- The response bytes contain the JSON with a "tag" field indicating the status
                  -- Parse it to determine the HTTP status code
                  let httpStatus = case Json.decodeBytes @CommandResponse responseBytes of
                        Result.Ok (CommandResponse.Accepted {}) -> HTTP.status200
                        Result.Ok (CommandResponse.Rejected {}) -> HTTP.status400
                        Result.Ok (CommandResponse.Failed {}) -> HTTP.status500
                        Result.Err _ -> HTTP.status500

                  let responseBody =
                        responseBytes
                          |> Bytes.toLazyLegacy
                          |> Wai.responseLBS httpStatus [(HTTP.hContentType, "application/json")]
                  respondValue <- respond responseBody
                  respondVar |> ConcurrentVar.set respondValue
                  Task.yield ()
              )
              |> Task.andThen
                ( \_ -> do
                    ConcurrentVar.get respondVar
                )
          Maybe.Nothing ->
            notFound [fmt|Command not found: #{commandName}|]
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
    let port = api.port
    Console.print [fmt|Starting WebApi server on port #{port}|]
    Warp.run api.port waiApp |> Task.fromIO


  buildCommandHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebApi ->
    Record.Proxy command ->
    (command -> Task Text CommandResponse) ->
    ApiEndpointHandler
  buildCommandHandler api _ handler body respond = do
    let port = api.port
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList

    -- Parse the command bytes using Aeson FromJSON
    let commandValue = body |> Json.decodeBytes @command

    case commandValue of
      Result.Ok cmd -> do
        -- Log that we're executing the command
        Console.print [fmt|Executing #{n} on port #{port}|]

        -- Execute the command and get the response
        response <- handler cmd
        let responseJson = Json.encodeText response |> Text.toBytes
        respond responseJson
      Result.Err _err -> do
        -- Handle parsing error - return a Failed response
        Console.print [fmt|Failed to parse command #{n} on port #{port}|]
        let errorResponse =
              CommandResponse.Failed
                { error = [fmt|Invalid JSON format for command #{n}|]
                }
        let responseJson = Json.encodeText errorResponse |> Text.toBytes
        respond responseJson