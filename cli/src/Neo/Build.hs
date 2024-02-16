module Neo.Build where

import Core
import JSON qualified


data Args = Args
  { name :: String
  }
  deriving (Generic)


-- TODO: Figure out easy parsing for any type. Perhaps some kind of TH schema definition that can be used for JSON, commands,
-- and whatever?
-- It'd be nice to have a single internal value-level type representation that people can leverage to write their own
-- formats. We could fork https://harry.garrood.me/blog/aeson-better-errors/

-- args :: Args.Parser Args
-- args = do
--   name <- Args.strOption (Args.long "name" <> Args.help "Your name" <> Args.metavar "NAME" <> Args.value "World")
--   pure (Args {name})

-- info :: Args.ParserInfo Args
-- info =
--   Args.info
--     (args <**> Args.helper)
--     ( Args.fullDesc <> Args.progDesc "Print a greeting for NAME" <> Args.header "hello - a test for optparse-applicative"
--     )

start :: Promise Void
start = do
  -- x <- Args.execParser info
  print "Hello, World!"