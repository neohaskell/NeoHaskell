module Neo (main) where

import Core
import Neo.Command qualified as Command
import Neo.Services qualified as Services
import OptionsParser qualified


main :: IO ()
main = do
  command <- OptionsParser.run Command.decoder
  services <- Services.create
  Command.handler services command
  Services.destroy services
