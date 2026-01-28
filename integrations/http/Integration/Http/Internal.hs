{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for HTTP outbound integration.
--
-- This module contains Nick's code - the 'ToAction' instance,
-- retry logic, environment variable expansion, and authentication
-- header building.
--
-- __This module is not exported to Jess.__
module Integration.Http.Internal
  ( -- * For Testing Only
    expandEnvVars
  , buildAuthHeader
  , calculateBackoff
  ) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import Bytes qualified
import Char (Char)
import Environment qualified
import LinkedList qualified
import Http.Client qualified as Http
import Int qualified
import Integration qualified
import Integration.Http.Auth (Auth (..))
import Integration.Http.Request (Body (..), Method (..), Request (..))
import Integration.Http.Response (Response (..))
import Integration.Http.Retry (Retry (..))
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (NameOf)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | ToAction instance that executes the HTTP request.
--
-- This is the main entry point - when Jess writes:
--
-- @
-- Integration.outbound Http.Request { ... }
-- @
--
-- This instance converts the config into an executable action.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config = Integration.action do
    executeRequest config


-- | Execute an HTTP request with retries and error handling.
executeRequest ::
  forall command.
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeRequest config = do
  -- Step 1: Expand environment variables in URL
  expandedUrl <- expandEnvVars config.url

  -- Step 2: Expand environment variables in headers
  expandedHeaders <- config.headers
    |> Task.mapArray (\(k, v) -> do
        expandedV <- expandEnvVars v
        Task.yield (k, expandedV))

  -- Step 3: Build authentication header
  authHeader <- buildAuthHeader config.auth

  -- Step 4: Combine headers with auth
  let allHeaders = case authHeader of
        Nothing -> expandedHeaders
        Just h -> expandedHeaders |> Array.push h

  -- Step 5: Execute with retries
  result <- executeWithRetry config.retry 1 do
    executeHttpRequest
      config.method
      expandedUrl
      allHeaders
      config.body
      config.timeoutSeconds

  -- Step 6: Handle result
  case result of
    Ok response ->
      Integration.emitCommand (config.onSuccess response)
    Err errorText ->
      case config.onError of
        Just handler -> Integration.emitCommand (handler errorText)
        Nothing -> Task.throw (Integration.NetworkError errorText)


-- | Execute HTTP request with exponential backoff retry.
executeWithRetry ::
  Retry ->
  Int ->                         -- Current attempt number
  Task Integration.IntegrationError (Result Text Response) ->
  Task Integration.IntegrationError (Result Text Response)
executeWithRetry retryConfig attempt action = do
  result <- action
    |> Task.asResult

  case result of
    Ok (Ok response) ->
      -- Success - but check if status is retryable (e.g., 429, 5xx)
      if isRetryableStatus retryConfig response.statusCode
           && attempt <= retryConfig.maxAttempts
        then do
          delay <- calculateBackoff retryConfig attempt
          AsyncTask.sleep delay
          executeWithRetry retryConfig (attempt + 1) action
        else
          Task.yield (Ok response)

    Ok (Err errorText) ->
      -- HTTP error - check if we should retry
      if attempt <= retryConfig.maxAttempts
        then do
          delay <- calculateBackoff retryConfig attempt
          AsyncTask.sleep delay
          executeWithRetry retryConfig (attempt + 1) action
        else
          Task.yield (Err errorText)

    Err integrationError ->
      -- Integration error - propagate immediately
      Task.throw integrationError


-- | Check if a status code should trigger a retry.
isRetryableStatus :: Retry -> Int -> Bool
isRetryableStatus config statusCode =
  config.retryableStatuses
    |> Array.any (== statusCode)


-- | Calculate backoff delay with exponential increase and jitter.
--
-- Formula: min(initialDelayMs * 2^(attempt-1), maxDelayMs) + jitter
-- Jitter: random value in [0, delay/4]
calculateBackoff :: Retry -> Int -> Task Integration.IntegrationError Int
calculateBackoff config attempt = do
  let multiplier = (attempt - 1) |> Int.powerOf 2
  let baseDelay = config.initialDelayMs * multiplier
  let cappedDelay = min baseDelay config.maxDelayMs
  -- Add jitter (0-25% of delay) to prevent thundering herd
  let jitterMax = max 1 (cappedDelay // 4)
  jitter <- Int.getRandomBetween 0 jitterMax
    |> Task.mapError (\_ -> Integration.UnexpectedError "Random generation failed")
  Task.yield (cappedDelay + jitter)


-- | Execute a single HTTP request.
executeHttpRequest ::
  Method ->
  Text ->
  Array (Text, Text) ->
  Body ->
  Int ->
  Task Integration.IntegrationError (Result Text Response)
executeHttpRequest method url headers body timeoutSeconds = do
  -- Build base request
  let baseRequest = Http.request
        |> Http.withUrl url
        |> Http.withTimeout timeoutSeconds

  -- Add headers
  let withHeaders =
        headers
          |> Array.foldl (\(key, value) acc -> acc |> Http.addHeader key value) baseRequest

  -- Helper to convert HTTP result to our result type
  let handleHttpResult result = case result of
        Ok jsonValue ->
          Ok Response
            { statusCode = 200  -- TODO: Extract from actual response when Http.Client supports it
            , body = jsonValue
            , headers = []      -- TODO: Extract from actual response when Http.Client supports it
            }
        Err httpErr ->
          Err (httpErrorToText httpErr)

  -- Execute based on method and body
  case (method, body) of
    (GET, _) -> do
      result <- Http.get @Json.Value withHeaders |> Task.asResult
      Task.yield (handleHttpResult result)

    (POST, JsonBody jsonBody) -> do
      result <- Http.post @Json.Value withHeaders jsonBody |> Task.asResult
      Task.yield (handleHttpResult result)

    (POST, FormBody formParams) -> do
      result <- Http.postForm @Json.Value withHeaders formParams |> Task.asResult
      Task.yield (handleHttpResult result)

    (POST, RawBody {}) ->
      -- TODO: Add raw body support to Http.Client
      Task.yield (Err "Raw body not yet supported")

    (POST, NoBody) -> do
      result <- Http.post @Json.Value withHeaders Json.null |> Task.asResult
      Task.yield (handleHttpResult result)

    (PUT, _) ->
      Task.yield (Err "PUT method not yet supported by Http.Client")

    (PATCH, _) ->
      Task.yield (Err "PATCH method not yet supported by Http.Client")

    (DELETE, _) ->
      Task.yield (Err "DELETE method not yet supported by Http.Client")


-- | Convert HTTP error to Text.
httpErrorToText :: Http.Error -> Text
httpErrorToText (Http.Error msg) = msg


-- | Expand environment variables in text.
--
-- Patterns like "${VAR_NAME}" are replaced with environment values.
-- Missing variables throw 'Integration.AuthenticationError'.
--
-- @
-- expandEnvVars "https://api.example.com/${API_VERSION}/orders"
-- -- With API_VERSION=v2: "https://api.example.com/v2/orders"
-- @
expandEnvVars :: Text -> Task Integration.IntegrationError Text
expandEnvVars text = do
  -- Find all ${VAR_NAME} patterns and expand them
  expandEnvVarsLoop text


-- | Loop through text expanding environment variables.
expandEnvVarsLoop :: Text -> Task Integration.IntegrationError Text
expandEnvVarsLoop text = do
  case findEnvVar text of
    Nothing ->
      -- No more variables to expand
      Task.yield text

    Just (before, varName, after) -> do
      -- Look up the environment variable
      value <- Environment.getVariable varName
        |> Task.mapError (\_ -> Integration.AuthenticationError
             [fmt|Missing environment variable: #{varName}|])

      -- Reconstruct and continue
      let expanded = Text.concat [before, value, after]
      expandEnvVarsLoop expanded


-- | Find the first ${VAR_NAME} pattern in text.
--
-- Returns (textBefore, varName, textAfter) if found.
findEnvVar :: Text -> Maybe (Text, Text, Text)
findEnvVar text = do
  let str = Text.toLinkedList text
  findEnvVarInList str [] Nothing


-- | Helper to find env var in character list.
findEnvVarInList :: [Char] -> [Char] -> Maybe [Char] -> Maybe (Text, Text, Text)
findEnvVarInList [] _before _inVar = Nothing
findEnvVarInList ('$':'{':rest) before Nothing =
  -- Start of variable
  findEnvVarInList rest before (Just [])
findEnvVarInList ('}':rest) before (Just varChars) =
  -- End of variable - success!
  let beforeText = Text.fromLinkedList (LinkedList.reverse before)
      varName = Text.fromLinkedList (LinkedList.reverse varChars)
      afterText = Text.fromLinkedList rest
  in Just (beforeText, varName, afterText)
findEnvVarInList (c:rest) before (Just varChars)
  | isVarChar c = findEnvVarInList rest before (Just (c:varChars))
  | otherwise = Nothing  -- Invalid character in variable name
findEnvVarInList (c:rest) before Nothing =
  findEnvVarInList rest (c:before) Nothing


-- | Check if a character is valid in an environment variable name.
isVarChar :: Char -> Bool
isVarChar c =
  (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '_'


-- | Build authentication header from Auth config.
--
-- All values are expanded for environment variables before use.
buildAuthHeader :: Auth -> Task Integration.IntegrationError (Maybe (Text, Text))
buildAuthHeader auth = case auth of
  NoAuth ->
    Task.yield Nothing

  Bearer token -> do
    expanded <- expandEnvVars token
    Task.yield (Just ("Authorization", [fmt|Bearer #{expanded}|]))

  Basic {username, password} -> do
    expandedUser <- expandEnvVars username
    expandedPass <- expandEnvVars password
    let credentials = base64Encode [fmt|#{expandedUser}:#{expandedPass}|]
    Task.yield (Just ("Authorization", [fmt|Basic #{credentials}|]))

  ApiKey {headerName, headerValue} -> do
    expandedValue <- expandEnvVars headerValue
    Task.yield (Just (headerName, expandedValue))


-- | Base64 encode text.
base64Encode :: Text -> Text
base64Encode text =
  text
    |> Text.toBytes
    |> Bytes.toBase64
    |> Text.fromBytes
