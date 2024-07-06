module Neo (main) where

import Core
import Neo.Command qualified as Command
import Neo.Services qualified as Services
import OptionsParser qualified


main :: IO ()
main = do
  command <- OptionsParser.run Command.decoder
  services <- Services.init
  Command.handler services command
