module Service.Transport.Mcp.JsonRpc (
  -- * Request types
  JsonRpcRequest (..),
  parseRequest,
  -- * Response types
  JsonRpcResponse (..),
  successResponse,
  errorResponse,
  -- * Error types
  JsonRpcError (..),
  -- * Standard error codes
  parseError,
  invalidRequest,
  methodNotFound,
  invalidParams,
  internalError,
  -- * Serialization
  encodeResponse,
) where

import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Appendable ((<>))
import Data.ByteString.Lazy qualified as LazyBytes
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)


-- | A parsed JSON-RPC 2.0 request.
data JsonRpcRequest = JsonRpcRequest
  { method :: Text
  , params :: Maybe Json.Value
  , id :: Maybe Json.Value
  }
  deriving (Show, Eq, Generic)


-- | A JSON-RPC 2.0 response.
data JsonRpcResponse = JsonRpcResponse
  { jsonrpc :: Text
  , result :: Maybe Json.Value
  , error :: Maybe JsonRpcError
  , id :: Maybe Json.Value
  }
  deriving (Show, Eq, Generic)


-- | A JSON-RPC 2.0 error object.
data JsonRpcError = JsonRpcError
  { code :: {-# UNPACK #-} Int
  , message :: Text
  , errorData :: Maybe Json.Value
  }
  deriving (Show, Eq, Generic)


-- | Parse a raw Bytes line into a JsonRpcRequest.
parseRequest :: Bytes -> Result JsonRpcResponse JsonRpcRequest
parseRequest bytes = do
  let rawBytes = Bytes.unwrap bytes
  case Aeson.decodeStrict' rawBytes of
    Nothing ->
      Err (errorResponse Nothing (parseError "Malformed JSON"))
    Just obj ->
      case AesonKeyMap.lookup (AesonKey.fromText "jsonrpc") obj of
        Nothing ->
          Err (errorResponse Nothing (invalidRequest "Missing jsonrpc field"))
        Just (Aeson.String version) ->
          if version == "2.0"
            then do
              let methodVal = AesonKeyMap.lookup (AesonKey.fromText "method") obj
              case methodVal of
                Just (Aeson.String m) -> do
                  let paramsVal = AesonKeyMap.lookup (AesonKey.fromText "params") obj
                  let idVal = AesonKeyMap.lookup (AesonKey.fromText "id") obj
                  Ok JsonRpcRequest
                    { method = m
                    , params = paramsVal
                    , id = idVal
                    }
                _ ->
                  Err (errorResponse Nothing (invalidRequest "Missing or invalid method field"))
            else
              Err (errorResponse Nothing (invalidRequest "Unsupported jsonrpc version"))
        _ ->
          Err (errorResponse Nothing (invalidRequest "Invalid jsonrpc field"))
{-# INLINE parseRequest #-}


-- | Construct a success response.
successResponse :: Maybe Json.Value -> Json.Value -> JsonRpcResponse
successResponse requestId resultValue = JsonRpcResponse
  { jsonrpc = "2.0"
  , result = Just resultValue
  , error = Nothing
  , id = requestId
  }
{-# INLINE successResponse #-}


-- | Construct an error response.
errorResponse :: Maybe Json.Value -> JsonRpcError -> JsonRpcResponse
errorResponse requestId err = JsonRpcResponse
  { jsonrpc = "2.0"
  , result = Nothing
  , error = Just err
  , id = requestId
  }
{-# INLINE errorResponse #-}


-- | -32700: Malformed JSON
parseError :: Text -> JsonRpcError
parseError msg = JsonRpcError
  { code = -32700
  , message = msg
  , errorData = Nothing
  }
{-# INLINE parseError #-}


-- | -32600: Invalid request
invalidRequest :: Text -> JsonRpcError
invalidRequest msg = JsonRpcError
  { code = -32600
  , message = msg
  , errorData = Nothing
  }
{-# INLINE invalidRequest #-}


-- | -32601: Unknown method
methodNotFound :: Text -> JsonRpcError
methodNotFound msg = JsonRpcError
  { code = -32601
  , message = msg
  , errorData = Nothing
  }
{-# INLINE methodNotFound #-}


-- | -32602: Invalid params
invalidParams :: Text -> JsonRpcError
invalidParams msg = JsonRpcError
  { code = -32602
  , message = msg
  , errorData = Nothing
  }
{-# INLINE invalidParams #-}


-- | -32603: Internal server error (generic message, never leaks details)
internalError :: JsonRpcError
internalError = JsonRpcError
  { code = -32603
  , message = "Internal server error"
  , errorData = Nothing
  }
{-# INLINE internalError #-}


-- | Serialize a JsonRpcResponse to Bytes with trailing newline.
encodeResponse :: JsonRpcResponse -> Bytes
encodeResponse response = do
  let obj = AesonKeyMap.fromList
        [ (AesonKey.fromText "jsonrpc", Aeson.String response.jsonrpc)
        , (AesonKey.fromText "id", case response.id of { Just v -> v; Nothing -> Aeson.Null })
        , case response.error of
            Just err ->
              (AesonKey.fromText "error", Json.object
                [ "code" Json..= err.code
                , "message" Json..= err.message
                ])
            Nothing ->
              (AesonKey.fromText "result", case response.result of { Just v -> v; Nothing -> Aeson.Null })
        ]
  let jsonBytes = Aeson.encode (Aeson.Object obj)
  let withNewline = jsonBytes <> LazyBytes.singleton 0x0A
  Bytes.fromLazyLegacy withNewline
{-# INLINE encodeResponse #-}
