module Starter.Config (
  StarterConfig (..),
  HasStarterConfig,
) where

import Config
import Core


defineConfig
  "StarterConfig"
  [ field @Int "httpPort"
      |> doc "HTTP server port"
      |> defaultsTo (8080 :: Int)
      |> envVar "PORT"
      |> cliLong "port"
      |> cliShort 'p'
  ]
