module Neo (main) where

import Console qualified
import Core
import Options.Applicative qualified as Opt
import OptionsParser qualified


data Sample = Sample
  { hello :: Text,
    quiet :: Bool,
    enthusiasm :: Int
  }


sample :: Opt.Parser Sample
sample = do
  hello <-
    Opt.strOption $
      Opt.long "hello"
        <> Opt.metavar "TARGET"
        <> Opt.help "Target for the greeting"

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

  yield $ Sample {hello, quiet, enthusiasm}


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


greet :: Sample -> IO ()
greet (Sample h False _) = print ("Hello, " ++ h ++ "!")
greet _ = print "Hi"