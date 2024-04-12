{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Neo.Build where

import Cli.Codec (CodecConfig (..), Options (..))
import Cli.Codec qualified as Codec
import Core
import Optional qualified
import Promise qualified
import Traits.Defaultable (with)


data Args = Args
  { name :: String,
    quiet :: Bool,
    enthusiasm :: Int
  }


schema :: Codec.Decoder Args
schema = Codec.do
  name <-
    Codec.text
      with
        { description = "Name to greet in the application",
          shorthand = 'n',
          defaultsTo = Optional.Some "World",
          placeholder = "NAME"
        }
  quiet <-
    Codec.bool
      with
        { name = "quiet",
          description = "Whether to be quiet",
          shorthand = 'q'
        }
  enthusiasm <-
    Codec.int
      with
        { name = "enthusiasm",
          description = "How enthusiastically to greet",
          shorthand = 'e',
          defaultsTo = Optional.Some 1
        }
  Codec.return Args {..}


greet :: Args -> Promise Void
greet args = Promise.do
  print ("Hello, " + args.name + "!")


init :: Promise Void
init = Promise.do
  args <-
    Codec.decode
      schema
      with
        { progDesc = "Print a greeting for NAME",
          header = "YAHAHOOOO"
        }
  greet args

{-
data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
-}