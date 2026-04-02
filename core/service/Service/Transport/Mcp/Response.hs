module Service.Transport.Mcp.Response (
  toCallToolResult,
  toResourceContent,
) where

import Array (Array)
import Array qualified
import Basics
import Json qualified
import Service.Response (CommandResponse (..))
import Text (Text)


-- | Convert a CommandResponse to an MCP CallToolResult JSON value.
toCallToolResult :: CommandResponse -> Json.Value
toCallToolResult response =
  case response of
    Accepted {entityId} -> do
      let confirmBlock = Json.object
            [ "type" Json..= ("text" :: Text)
            , "text" Json..= ([fmt|Command accepted. Entity ID: #{entityId}|] :: Text)
            , "annotations" Json..= Json.object
                [ "audience" Json..= Json.toJSON (Array.fromLinkedList ["user", "assistant"] :: Array Text)
                ]
            ]
      let guidanceBlock = Json.object
            [ "type" Json..= ("text" :: Text)
            , "text" Json..= ([fmt|This command registered an event but does not return entity state. To see the current state, read the appropriate resource (e.g. resources/read with URI neohaskell://queries/{query-name}?id=#{entityId}).|] :: Text)
            , "annotations" Json..= Json.object
                [ "audience" Json..= Json.toJSON (Array.fromLinkedList ["assistant"] :: Array Text)
                , "priority" Json..= Json.toJSON (0.9 :: Float)
                ]
            ]
      let contentBlocks = Array.fromLinkedList [confirmBlock, guidanceBlock]
      Json.object
        [ "content" Json..= Json.toJSON contentBlocks
        , "isError" Json..= Json.toJSON False
        ]
    Rejected {reason} -> do
      let block = Json.object
            [ "type" Json..= ("text" :: Text)
            , "text" Json..= ([fmt|Command rejected: #{reason}|] :: Text)
            ]
      let contentBlocks = Array.fromLinkedList [block] :: Array Json.Value
      Json.object
        [ "content" Json..= Json.toJSON contentBlocks
        , "isError" Json..= Json.toJSON True
        ]
    Failed {} -> do
      let block = Json.object
            [ "type" Json..= ("text" :: Text)
            , "text" Json..= ("Command failed: Internal error" :: Text)
            ]
      let contentBlocks = Array.fromLinkedList [block] :: Array Json.Value
      Json.object
        [ "content" Json..= Json.toJSON contentBlocks
        , "isError" Json..= Json.toJSON True
        ]
{-# INLINE toCallToolResult #-}


-- | Wrap query result text as an MCP resource content JSON value.
toResourceContent :: Text -> Text -> Json.Value
toResourceContent resourceUri queryResultJson = do
  let entry = Json.object
        [ "uri" Json..= resourceUri
        , "mimeType" Json..= ("application/json" :: Text)
        , "text" Json..= queryResultJson
        ]
  let entries = Array.fromLinkedList [entry] :: Array Json.Value
  Json.object
    [ "contents" Json..= Json.toJSON entries
    ]
{-# INLINE toResourceContent #-}
