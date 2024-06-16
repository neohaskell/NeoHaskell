{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Neo (main) where

import Applicable (Applicative (pure))
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
          help = "Target for the greeting"
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
          showDefault = True,
          value = 1
        }

  ANON {hello = hello, quiet = quiet, enthusiasm = enthusiasm}
    |> pure


main :: IO ()
main = do
  aaa <-
    OptionsParser.run
      sample
      ANON
        { header = "hello - a test for optparse-applicative",
          description = "Print a greeting for TARGET"
        }
  greet aaa


greet :: Sample -> IO ()
greet opts
  | opts.quiet = print "shhh, hi!"
  | otherwise = print ("Hello, " ++ opts.hello ++ "!")