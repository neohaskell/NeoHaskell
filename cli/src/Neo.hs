{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Neo (main) where

import Core
import Options.Applicative qualified as Opt
import OptionsParser (OptionsParser)
import OptionsParser qualified


data ProgramOptions = ProgramOptions
  { hello :: Text,
    quiet :: Bool,
    enthusiasm :: Int
  }


programOptionsParser :: OptionsParser ProgramOptions
programOptionsParser = do
  hello <-
    OptionsParser.text
      ANON
        { long = "hello",
          metavar = "TARGET",
          help = "Target for the greeting"
        }

  quiet <-
    Opt.switch $
      Opt.long "quiet"
        <> Opt.short 'q'
        <> Opt.help "Whether to be quiet"

  enthusiasm <-
    Opt.option Opt.auto $
      Opt.long "enthusiasm"
        <> Opt.help "How enthusiastically to greet"
        <> Opt.showDefault
        <> Opt.value 1
        <> Opt.metavar "INT"

  yield ProgramOptions {hello, quiet, enthusiasm}


-- main :: IO Unit
-- main = Console.print "Hello world!"

main :: IO ()
main = greet =<< execParser opts
 where
  opts =
    info
      (sample <**> helper)
      ( fullDesc
          <> progDesc "Print a greeting for TARGET"
          <> header "hello - a test for optparse-applicative"
      )


greet :: ProgramOptions -> IO ()
greet (ProgramOptions h False _) = print ("Hello, " ++ h ++ "!")
greet _ = print "Hi"