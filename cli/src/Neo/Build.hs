{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Neo.Build where

import Cli.Codec qualified as Codec
import Core
import Data.Monoid qualified as Monoid
import Data.String qualified as GHC
import HaskellCompatibility.Conversion qualified as Convert
import Options.Applicative
import Options.Applicative qualified as OptParse
import Options.Applicative.Types qualified as OptParse
import Promise qualified
import Unsafe.Coerce (unsafeCoerce)


data Args = Args
  { name :: String,
    quiet :: Bool,
    enthusiasm :: Int
  }


schema :: Codec.Decoder Args
schema = Codec.do
  name <-
    Schema.text
      { description = "Name to greet in the application",
        shorthand = 'n',
        defaultsTo = "World"
      }
  quiet <-
    Schema.bool
      { name = "quiet",
        description = "Whether to be quiet",
        shorthand = 'q',
        defaultsTo = False
      }
  enthusiasm <-
    Schema.int
      { name = "enthusiasm",
        description = "How enthusiastically to greet",
        shorthand = 'e',
        defaultsTo = 1
      }
  Schema.defines Args {..}


greet :: Args -> Promise Void
greet args = do
  print ("Hello, " + args.name + "!")

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