{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Testbed.Integrations.JsonSideEffect (
  JsonSideEffect (..),
) where

import Array (Array)
import Basics
import Integration qualified
import Integration.Http qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Service.Command.Core (NameOf)
import Text (Text)


data JsonSideEffect payload command = JsonSideEffect
  { method :: Http.Method,
    url :: Text,
    headers :: Array (Text, Text),
    payload :: payload,
    auth :: Http.Auth,
    retry :: Http.Retry,
    timeoutSeconds :: Int,
    onSuccess :: Http.Response -> command,
    onError :: Text -> command
  }


instance
  ( Json.ToJSON payload,
    Json.ToJSON command,
    KnownSymbol (NameOf command)
  ) =>
  Integration.ToAction (JsonSideEffect payload command)
  where
  toAction config =
    Integration.outbound
      Http.Request
        { method = config.method,
          url = config.url,
          headers = config.headers,
          body = Http.json config.payload,
          onSuccess = config.onSuccess,
          onError = Just config.onError,
          auth = config.auth,
          retry = config.retry,
          timeoutSeconds = config.timeoutSeconds
        }
