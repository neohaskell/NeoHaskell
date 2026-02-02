-- | Built-in endpoint schemas for WebTransport's non-command/query endpoints.
--
-- These schemas document the OAuth2 authentication and file upload endpoints
-- that are automatically added by WebTransport when configured.
--
-- Used by OpenAPI generation to include all endpoints in the API documentation.
module Service.Transport.Web.BuiltinSchemas (
  oauth2EndpointSchemas,
  fileUploadEndpointSchemas,
) where

import Array qualified
import Basics
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Schema (FieldSchema (..), Schema (..))
import Service.Transport (EndpointSchema (..))
import Text (Text)


-- | Generate endpoint schemas for OAuth2 routes.
--
-- Returns schemas for /connect/{provider}, /callback/{provider}, /disconnect/{provider}
-- when OAuth2 is configured. Returns empty map when OAuth2 is not configured.
--
-- All OAuth2 endpoints are tagged under "Authentication".
oauth2EndpointSchemas :: Bool -> Map Text EndpointSchema
oauth2EndpointSchemas isEnabled =
  if not isEnabled
    then Map.empty
    else
      Map.empty
        |> Map.set "connect" connectSchema
        |> Map.set "callback" callbackSchema
        |> Map.set "disconnect" disconnectSchema


-- | Generate endpoint schemas for file upload routes.
--
-- Returns schemas for POST /files/upload and GET /files/{fileRef}
-- when file uploads are configured. Returns empty map when not configured.
--
-- All file endpoints are tagged under "Files".
fileUploadEndpointSchemas :: Bool -> Map Text EndpointSchema
fileUploadEndpointSchemas isEnabled =
  if not isEnabled
    then Map.empty
    else
      Map.empty
        |> Map.set "upload" uploadSchema
        |> Map.set "download" downloadSchema


-- -----------------------------------------------------------------------------
-- OAuth2 Schemas
-- -----------------------------------------------------------------------------

-- | Schema for GET /connect/{provider}
-- Initiates OAuth2 authorization flow, returns 302 redirect.
connectSchema :: EndpointSchema
connectSchema = EndpointSchema
  { requestSchema = Nothing  -- GET with no body
  , responseSchema = redirectResponseSchema
  , description = "Initiate OAuth2 connection flow. Redirects to provider's authorization page."
  , deprecated = False
  , entityName = Just "Authentication"
  }


-- | Schema for GET /callback/{provider}
-- Handles OAuth2 callback with code and state parameters.
callbackSchema :: EndpointSchema
callbackSchema = EndpointSchema
  { requestSchema = Nothing  -- GET with query params
  , responseSchema = redirectResponseSchema
  , description = "OAuth2 callback handler. Exchanges code for tokens and redirects to success/failure URL."
  , deprecated = False
  , entityName = Just "Authentication"
  }


-- | Schema for DELETE /disconnect/{provider}
-- Disconnects an OAuth2 provider from user's account.
disconnectSchema :: EndpointSchema
disconnectSchema = EndpointSchema
  { requestSchema = Nothing  -- DELETE with no body
  , responseSchema = disconnectResponseSchema
  , description = "Disconnect OAuth2 provider from user account."
  , deprecated = False
  , entityName = Just "Authentication"
  }


-- -----------------------------------------------------------------------------
-- File Upload Schemas
-- -----------------------------------------------------------------------------

-- | Schema for POST /files/upload
-- Accepts multipart/form-data file upload.
uploadSchema :: EndpointSchema
uploadSchema = EndpointSchema
  { requestSchema = Just multipartFormDataSchema
  , responseSchema = fileRefResponseSchema
  , description = "Upload a file. Accepts multipart/form-data with file content."
  , deprecated = False
  , entityName = Just "Files"
  }


-- | Schema for GET /files/{fileRef}
-- Downloads a file by its reference.
downloadSchema :: EndpointSchema
downloadSchema = EndpointSchema
  { requestSchema = Nothing  -- GET with path param
  , responseSchema = SText  -- Binary content (simplified)
  , description = "Download a file by its reference. Returns file content with appropriate Content-Type."
  , deprecated = False
  , entityName = Just "Files"
  }


-- -----------------------------------------------------------------------------
-- Response Schemas
-- -----------------------------------------------------------------------------

-- | Schema for 302 redirect responses (empty body)
redirectResponseSchema :: Schema
redirectResponseSchema = SObject Array.empty


-- | Schema for disconnect response
disconnectResponseSchema :: Schema
disconnectResponseSchema = SObject (Array.fromLinkedList
  [ FieldSchema
      { fieldName = "status"
      , fieldSchema = SText
      , fieldRequired = True
      , fieldDescription = "Disconnection status (e.g., 'disconnected')"
      }
  , FieldSchema
      { fieldName = "provider"
      , fieldSchema = SText
      , fieldRequired = True
      , fieldDescription = "Provider that was disconnected"
      }
  ])


-- | Schema for multipart form data (simplified for OpenAPI)
-- In practice, this is handled specially by OpenAPI as multipart/form-data
multipartFormDataSchema :: Schema
multipartFormDataSchema = SObject (Array.fromLinkedList
  [ FieldSchema
      { fieldName = "file"
      , fieldSchema = SText  -- Binary format
      , fieldRequired = True
      , fieldDescription = "File content (binary)"
      }
  ])


-- | Schema for file upload response
fileRefResponseSchema :: Schema
fileRefResponseSchema = SObject (Array.fromLinkedList
  [ FieldSchema
      { fieldName = "fileRef"
      , fieldSchema = SText
      , fieldRequired = True
      , fieldDescription = "Unique file reference for downloading"
      }
  , FieldSchema
      { fieldName = "filename"
      , fieldSchema = SText
      , fieldRequired = True
      , fieldDescription = "Original filename"
      }
  , FieldSchema
      { fieldName = "contentType"
      , fieldSchema = SText
      , fieldRequired = True
      , fieldDescription = "MIME type of the uploaded file"
      }
  , FieldSchema
      { fieldName = "size"
      , fieldSchema = SInt
      , fieldRequired = True
      , fieldDescription = "File size in bytes"
      }
  ])
