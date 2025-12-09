{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Service.Apis.WebApi (
  -- * Web API
  WebApi (..),
  server,
) where

import Basics
import Record (KnownHash (..))
import Service.CommandHandler.TH (deriveKnownHash)
import Service.Protocol (ServerApi (..))
import Text (Text)


-- | WebApi type - carries HTTP API configuration
data WebApi = WebApi
  { -- | HTTP port to listen on
    port :: Int,
    -- | Host to bind to
    host :: Text
  }
  deriving (Eq, Show, Generic)


-- | Default WebApi configuration
server :: WebApi
server =
  WebApi
    { port = 8080,
      host = "localhost"
    }


-- | WebApi is a server API for HTTP-based communication
instance ServerApi WebApi where
  type ServerName WebApi = "WebApi"
  type ApiConfig WebApi = WebApi
  type ApiState WebApi = ()


deriveKnownHash "WebApi"
