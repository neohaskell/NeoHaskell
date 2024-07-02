module Neo (main) where

import Core
import OptionsParser (OptionsParser)
import OptionsParser qualified


type Sample =
  Record
    [ "hello" := Text,
      "quiet" := Bool,
      "enthusiasm" := Int
    ]


sample :: OptionsParser Sample
sample = do
  hello <-
    OptionsParser.text
      ANON
        { long = "hello",
          metavar = "TARGET",
          help = "Target for the greeting",
          short = 'h'
        }

  quiet <-
    OptionsParser.flag
      ANON
        { long = "quiet",
          short = 'q',
          help = "Whether to be quiet"
        }

  enthusiasm <-
    OptionsParser.json
      ANON
        { long = "enthusiasm",
          metavar = "INT",
          help = "How enthusiastically to greet",
          short = 'e',
          value = Just 1
        }

  pure (ANON {hello = hello, quiet = quiet, enthusiasm = enthusiasm})


main :: IO ()
main = do
  aaa <- OptionsParser.run sample
  greet aaa


greet :: Sample -> IO ()
greet opts
  | opts.quiet = print "shhh, hi!"
  | otherwise = print ("Hello, " ++ opts.hello ++ "!")