{- HLINT ignore "Use lambda-case" -}
module Integration.Agent.InternalSpec (spec) where

import Array (Array)
import Array qualified
import Basics
import Integration.Agent.Internal
import Integration.OpenRouter.Internal (RequestBody (..))
import Integration.Agent qualified as Agent
import Integration.Agent.TestFixtures
import Integration.Agent.Types (CommandTool (..), Config (..), Request (..), defaultConfig)
import Integration.Http.Response (Response (..))
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response (Choice (..), Response (..), ToolCall (..), ToolCallFunction (..))
import Integration.OpenRouter.Response qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result (Result (..))
import Result qualified
import Test.Hspec
import Text (Text)
import Text qualified


-- | Build a synthetic HTTP response for testing.
makeHttpResponse :: Int -> Json.Value -> Integration.Http.Response.Response
makeHttpResponse statusCode body =
  Integration.Http.Response.Response
    { statusCode = statusCode
    , body = body
    , headers = Array.empty
    }


-- | Build a minimal valid OpenRouter Response with one tool call.
makeResponseWithToolCall :: Text -> Text -> OpenRouter.Response
makeResponseWithToolCall toolName argsText =
  OpenRouter.Response
    { id = "gen-test"
    , model = "test-model"
    , choices = Array.fromLinkedList
        [ Choice
            { message = Message.assistant ""
            , finishReason = OpenRouter.Stop
            , index = 0
            , toolCalls = Array.fromLinkedList
                [ ToolCall
                    { id = "call_1"
                    , function = ToolCallFunction
                        { name = toolName
                        , arguments = Redacted.wrap argsText
                        }
                    }
                ]
            }
        ]
    , usage = Nothing
    }


-- | Build a minimal valid OpenRouter Response with no tool calls.
makeResponseNoToolCalls :: OpenRouter.Response
makeResponseNoToolCalls =
  OpenRouter.Response
    { id = "gen-test"
    , model = "test-model"
    , choices = Array.fromLinkedList
        [ Choice
            { message = Message.assistant "No tool"
            , finishReason = OpenRouter.Stop
            , index = 0
            , toolCalls = Array.empty
            }
        ]
    , usage = Nothing
    }


-- | Build a minimal agent request for handleAgentHttpSuccess tests.
makeAgentRequest :: Array CommandTool -> (Text -> AgentCommand) -> Request AgentCommand
makeAgentRequest tools onErrorFn =
  Request
    { prompt = "test prompt"
    , tools = tools
    , model = "test-model"
    , config = defaultConfig
    , onError = onErrorFn
    }


spec :: Spec
spec = do
  describe "Integration.Agent.Internal" do
    describe "buildMessages" do
      it "no system prompt produces single user message" do
        let messages = buildMessages defaultConfig "Hello"
        Array.length messages `shouldBe` 1
        case Array.first messages of
          Just msg -> msg.role `shouldBe` Message.User
          Nothing -> expectationFailure "Expected one message"

      it "system prompt prepended as first message then user message" do
        let config = defaultConfig {systemPrompt = Just "You are strict."}
        let messages = buildMessages config "Hello"
        Array.length messages `shouldBe` 2
        case Array.first messages of
          Just first -> first.role `shouldBe` Message.System
          Nothing -> expectationFailure "Expected system message first"
        case messages |> Array.drop 1 |> Array.first of
          Just second -> second.role `shouldBe` Message.User
          Nothing -> expectationFailure "Expected user message second"

      it "empty system prompt string still produces system message" do
        let config = defaultConfig {systemPrompt = Just ""}
        let messages = buildMessages config "Hello"
        Array.length messages `shouldBe` 2
        case Array.first messages of
          Just first -> first.role `shouldBe` Message.System
          Nothing -> expectationFailure "Expected system message first"

    describe "parseFirstToolCall" do
      it "no choices returns error: No choices in OpenRouter response" do
        let response = OpenRouter.Response
              { id = "gen-1"
              , model = "m"
              , choices = Array.empty
              , usage = Nothing
              }
        let result = parseFirstToolCall response
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> msg `shouldBe` "No choices in OpenRouter response"
          Ok _ -> expectationFailure "Expected Err"

      it "first choice with no tool calls returns error: Model did not return a tool call" do
        let response = makeResponseNoToolCalls
        let result = parseFirstToolCall response
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> msg `shouldBe` "Model did not return a tool call"
          Ok _ -> expectationFailure "Expected Err"

      it "returns first tool call from first choice" do
        let response = makeResponseWithToolCall "AddItem" validArgsJson
        let result = parseFirstToolCall response
        (Result.isOk result) `shouldBe` True
        case result of
          Ok toolCall -> toolCall.function.name `shouldBe` "AddItem"
          Err err -> expectationFailure (Text.toLinkedList [fmt|Expected Ok, got Err: #{err}|])

      it "ignores later choices - uses only first choice" do
        let response = OpenRouter.Response
              { id = "gen-1"
              , model = "m"
              , choices = Array.fromLinkedList
                  [ Choice
                      { message = Message.assistant ""
                      , finishReason = OpenRouter.Stop
                      , index = 0
                      , toolCalls = Array.empty  -- first choice has no tool calls
                      }
                  , Choice
                      { message = Message.assistant ""
                      , finishReason = OpenRouter.Stop
                      , index = 1
                      , toolCalls = Array.fromLinkedList
                          [ ToolCall
                              { id = "call_b"
                              , function = ToolCallFunction
                                  { name = "CallB"
                                  , arguments = Redacted.wrap ("{}" :: Text)
                                  }
                              }
                          ]
                      }
                  ]
              , usage = Nothing
              }
        let result = parseFirstToolCall response
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> msg `shouldBe` "Model did not return a tool call"
          Ok _ -> expectationFailure "Expected Err from empty first choice"

      it "ignores additional calls in first choice - returns only first" do
        let response = OpenRouter.Response
              { id = "gen-1"
              , model = "m"
              , choices = Array.fromLinkedList
                  [ Choice
                      { message = Message.assistant ""
                      , finishReason = OpenRouter.Stop
                      , index = 0
                      , toolCalls = Array.fromLinkedList
                          [ ToolCall { id = "call_a", function = ToolCallFunction { name = "CallA", arguments = Redacted.wrap ("{}" :: Text) } }
                          , ToolCall { id = "call_b", function = ToolCallFunction { name = "CallB", arguments = Redacted.wrap ("{}" :: Text) } }
                          ]
                      }
                  ]
              , usage = Nothing
              }
        let result = parseFirstToolCall response
        (Result.isOk result) `shouldBe` True
        case result of
          Ok toolCall -> toolCall.function.name `shouldBe` "CallA"
          Err err -> expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])

    describe "validateToolName" do
      it "registered tool name is accepted unchanged" do
        let tool = CommandTool { toolName = "AddItem", toolDescription = "desc", toolDefinition = Json.null }
        let tools = Array.fromLinkedList [tool]
        let toolCall = ToolCall { id = "call_1", function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) } }
        let result = validateToolName tools toolCall
        (Result.isOk result) `shouldBe` True

      it "unregistered tool name returns error with tool name" do
        let tools = Array.fromLinkedList
              [ CommandTool { toolName = "AddItem", toolDescription = "", toolDefinition = Json.null }
              , CommandTool { toolName = "RemoveItem", toolDescription = "", toolDefinition = Json.null }
              ]
        let toolCall = ToolCall { id = "call_1", function = ToolCallFunction { name = "DeleteAll", arguments = Redacted.wrap ("{}" :: Text) } }
        let result = validateToolName tools toolCall
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> Text.contains "DeleteAll" msg `shouldBe` True
          Ok _ -> expectationFailure "Expected Err"

      it "long unregistered name is truncated to 100 chars in error message" do
        let tools = Array.empty
        let toolCall = ToolCall { id = "call_1", function = ToolCallFunction { name = longName, arguments = Redacted.wrap ("{}" :: Text) } }
        let result = validateToolName tools toolCall
        case result of
          Err msg ->
            -- 200 = max 100 chars for truncated tool name + ~100 chars for error prefix and formatting
            Text.length msg `shouldSatisfy` (\n -> n <= 200)
          Ok _ -> expectationFailure "Expected Err for unregistered long name"

      it "name matching is case-sensitive" do
        let tools = Array.fromLinkedList
              [ CommandTool { toolName = "AddItem", toolDescription = "", toolDefinition = Json.null }
              ]
        let toolCall = ToolCall { id = "call_1", function = ToolCallFunction { name = "additem", arguments = Redacted.wrap ("{}" :: Text) } }
        let result = validateToolName tools toolCall
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> Text.contains "additem" msg `shouldBe` True
          Ok _ -> expectationFailure "Expected Err for case-mismatched name"

      it "empty registered set rejects any tool" do
        let tools = Array.empty
        let toolCall = ToolCall { id = "call_1", function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) } }
        let result = validateToolName tools toolCall
        (Result.isErr result) `shouldBe` True

    describe "decodeArguments" do
      it "valid command JSON decodes to correct value" do
        let toolCall = ToolCall
              { id = "call_1"
              , function = ToolCallFunction
                  { name = "AddItem"
                  , arguments = Redacted.wrap validArgsJson
                  }
              }
        let result = decodeArguments @AddItemCommand toolCall
        (Result.isOk result) `shouldBe` True
        case result of
          Ok cmd ->
            cmd `shouldBe` AddItemCommand
              { cartId = "cart-1"
              , stockId = "stock-1"
              , quantity = 2
              }
          Err err -> expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])

      it "malformed JSON returns error with tool name" do
        let toolCall = ToolCall
              { id = "call_1"
              , function = ToolCallFunction
                  { name = "AddItem"
                  , arguments = Redacted.wrap malformedArgsJson
                  }
              }
        let result = decodeArguments @AddItemCommand toolCall
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> Text.contains "AddItem" msg `shouldBe` True
          Ok _ -> expectationFailure "Expected Err for malformed JSON"

      it "missing field returns same decode error" do
        let argsWithMissingField = "{\"cartId\":\"cart-1\",\"stockId\":\"stock-1\"}"
        let toolCall = ToolCall
              { id = "call_1"
              , function = ToolCallFunction
                  { name = "AddItem"
                  , arguments = Redacted.wrap argsWithMissingField
                  }
              }
        let result = decodeArguments @AddItemCommand toolCall
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> Text.contains "AddItem" msg `shouldBe` True
          Ok _ -> expectationFailure "Expected Err for missing field"

      it "wrong field type returns same decode error" do
        let argsWrongType = "{\"cartId\":\"cart-1\",\"stockId\":\"stock-1\",\"quantity\":\"two\"}"
        let toolCall = ToolCall
              { id = "call_1"
              , function = ToolCallFunction
                  { name = "AddItem"
                  , arguments = Redacted.wrap argsWrongType
                  }
              }
        let result = decodeArguments @AddItemCommand toolCall
        (Result.isErr result) `shouldBe` True
        case result of
          Err msg -> Text.contains "AddItem" msg `shouldBe` True
          Ok _ -> expectationFailure "Expected Err for wrong field type"

      it "long tool name is truncated in error message" do
        let toolCall = ToolCall
              { id = "call_1"
              , function = ToolCallFunction
                  { name = longName
                  , arguments = Redacted.wrap malformedArgsJson
                  }
              }
        let result = decodeArguments @AddItemCommand toolCall
        case result of
          Err msg ->
            -- error message prefix + truncated name (100 chars)
            Text.length msg `shouldSatisfy` (\n -> n <= 200)
          Ok _ -> expectationFailure "Expected Err for long name with bad args"

    describe "buildAgentRequestBody" do
      it "always sets tools and tool_choice=required" do
        let tool = Agent.commandTool @AddItemCommand
        let req = makeAgentRequest (Array.fromLinkedList [tool]) CommandError
        let (body :: RequestBody) = buildAgentRequestBody req
        body.tools `shouldSatisfy` (\t -> case t of { Just _ -> True; Nothing -> False })
        body.tool_choice `shouldBe` Just "required"
        body.stream `shouldBe` False

      it "system prompt contributes first message" do
        let config = defaultConfig {systemPrompt = Just "You are strict."}
        let tool = Agent.commandTool @AddItemCommand
        let req = Request
              { prompt = "test"
              , tools = Array.fromLinkedList [tool]
              , model = "m"
              , config = config
              , onError = CommandError
              }
        let (body :: RequestBody) = buildAgentRequestBody req
        Array.length body.messages `shouldBe` 2
        case Array.first body.messages of
          Just first -> first.role `shouldBe` Message.System
          Nothing -> expectationFailure "Expected system message"

      it "temperature and maxTokens propagate to body" do
        let config = defaultConfig {temperature = Just 0.2, maxTokens = Just 512}
        let req = Request
              { prompt = "test"
              , tools = Array.empty
              , model = "m"
              , config = config
              , onError = CommandError
              }
        let (body :: RequestBody) = buildAgentRequestBody req
        body.temperature `shouldBe` (Just 0.2 :: Maybe Float)
        body.max_tokens `shouldBe` (Just 512 :: Maybe Int)

      it "tool order is preserved in request body" do
        let toolA = CommandTool { toolName = "A", toolDescription = "a", toolDefinition = Json.object [("name", Json.toJSON ("A" :: Text))] }
        let toolB = CommandTool { toolName = "B", toolDescription = "b", toolDefinition = Json.object [("name", Json.toJSON ("B" :: Text))] }
        let req = makeAgentRequest (Array.fromLinkedList [toolA, toolB]) CommandError
        let (body :: RequestBody) = buildAgentRequestBody req
        case body.tools of
          Just tools -> Array.length tools `shouldBe` 2
          Nothing -> expectationFailure "Expected tools to be set"

      it "empty tool array is still explicit" do
        let req = makeAgentRequest Array.empty CommandError
        let (body :: RequestBody) = buildAgentRequestBody req
        body.tools `shouldBe` (Just (Array.empty :: Array Json.Value))
        body.tool_choice `shouldBe` Just "required"

    describe "handleAgentHttpSuccess" do
      let tools = Array.fromLinkedList [CommandTool { toolName = "AddItem", toolDescription = "d", toolDefinition = Json.null }]
      let req = makeAgentRequest tools CommandError

      it "parse failure on 200 returns sanitized error" do
        let httpResp = makeHttpResponse 200 Json.null
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "Failed to parse OpenRouter response"

      it "HTTP 429 returns rate limit error" do
        let httpResp = makeHttpResponse 429 Json.null
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "OpenRouter rate limit exceeded"

      it "HTTP 400 returns client error" do
        let httpResp = makeHttpResponse 400 Json.null
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "OpenRouter request error (HTTP 400)"

      it "HTTP 404 returns client error with code" do
        let httpResp = makeHttpResponse 404 Json.null
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "OpenRouter request error (HTTP 404)"

      it "HTTP 503 returns server error" do
        let httpResp = makeHttpResponse 503 Json.null
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "OpenRouter server error (HTTP 503)"

      it "200 with no choices returns no-choices error" do
        let emptyChoicesResponse = OpenRouter.Response
              { id = "gen-1", model = "m"
              , choices = Array.empty
              , usage = Nothing
              }
        let body = Json.encode emptyChoicesResponse
        let httpResp = makeHttpResponse 200 body
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "No choices in OpenRouter response"

      it "200 with choice having no tool calls returns no-tool-call error" do
        let noToolCallResponse = makeResponseNoToolCalls
        let body = Json.encode noToolCallResponse
        let httpResp = makeHttpResponse 200 body
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "Model did not return a tool call"

      it "unregistered tool name returns unregistered-tool error" do
        let response = makeResponseWithToolCall "DeleteAll" "{}"
        let body = Json.encode response
        let httpResp = makeHttpResponse 200 body
        let result = handleAgentHttpSuccess req httpResp
        result `shouldBe` CommandError "Agent returned unregistered tool: DeleteAll"

      it "registered tool with malformed args returns decode error" do
        let response = makeResponseWithToolCall "AddItem" malformedArgsJson
        let body = Json.encode response
        let httpResp = makeHttpResponse 200 body
        let result = handleAgentHttpSuccess @AgentCommand (makeAgentRequest tools (\err -> CommandError err)) httpResp
        result `shouldBe` CommandError "Failed to decode tool arguments for AddItem"

      it "full happy path: 200 with valid tool and args returns decoded command" do
        -- Build JSON body directly (not through ToJSON) to simulate real API response
        -- with actual (non-redacted) arguments, as OpenRouter would return.
        let toolCallJson = Json.object
              [ ("id", Json.toJSON ("call_1" :: Text))
              , ("function", Json.object
                  [ ("name", Json.toJSON ("AddItem" :: Text))
                  , ("arguments", Json.toJSON validArgsJson)
                  ])
              ]
        let choiceJson = Json.object
              [ ("message", Json.object
                  [ ("role", Json.toJSON ("assistant" :: Text))
                  , ("content", Json.toJSON ("" :: Text))
                  ])
              , ("finish_reason", Json.toJSON ("stop" :: Text))
              , ("index", Json.toJSON (0 :: Int))
              , ("tool_calls", Json.toJSON ([toolCallJson] :: [Json.Value]))
              ]
        let body = Json.object
              [ ("id", Json.toJSON ("gen-test" :: Text))
              , ("model", Json.toJSON ("test-model" :: Text))
              , ("choices", Json.toJSON ([choiceJson] :: [Json.Value]))
              ]
        let httpResp = makeHttpResponse 200 body
        let result = handleAgentHttpSuccess @AgentCommand (makeAgentRequest tools (\err -> CommandError err)) httpResp
        result `shouldBe` CommandOk (AddItemCommand { cartId = "cart-1", stockId = "stock-1", quantity = 2 })

    describe "handleAgentHttpError" do
      let req = makeAgentRequest Array.empty CommandError

      it "passes through provider error text unchanged" do
        let result = handleAgentHttpError req "connection reset by peer"
        result `shouldBe` CommandError "connection reset by peer"

      it "passes through structured provider message" do
        let result = handleAgentHttpError req "HTTP client timeout after 30s"
        result `shouldBe` CommandError "HTTP client timeout after 30s"

      it "empty error text remains empty" do
        let result = handleAgentHttpError req ""
        result `shouldBe` CommandError ""

    describe "executeAgent (preflight)" do
      it "empty tools list is rejected before HTTP dispatch" do
        pendingWith "requires ActionContext setup"

      it "empty tools rejection is deterministic for whitespace prompt" do
        pendingWith "requires ActionContext setup"

      it "empty tools rejection ignores tuning config" do
        pendingWith "requires ActionContext setup"
