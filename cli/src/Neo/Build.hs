{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Neo.Build where

import Core
import HaskellCompatibility.Conversion qualified as Convert
import Options.Applicative
import Options.Applicative.Types qualified as OptParse
import Promise qualified
import Schema qualified
import Schema.Types


data Args = Args
  { name :: String,
    quiet :: Bool,
    enthusiasm :: Int
  }


schema :: Schema Args
schema = Schema.do
  name <-
    Schema.text
      { name = "name",
        description = "Name to greet in the application",
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


toParser :: Schema a -> Parser a
toParser s = case s of
  Schema.NullSchema o -> OptParse.NilP (Convert.toLegacy o)
  Schema.PropertySchema options -> OptParse.OptP (makeOptP options)
  Schema.PartialSchema f innerSchema -> OptParse.MultP (toParser f) (toParser innerSchema)
  Schema.AlternativeSchema left right -> OptParse.AltP (toParser left) (toParser right)
  Schema.ContinuationSchema innerSchema f -> OptParse.BindP (toParser innerSchema) (f .> toParser)


makeOptP :: a
makeOptP = todo -- TODO: Implement makeOptP


start :: Promise Void
start = Promise.do
  let opts = todo
  -- info
  --   (sample <**> helper)
  --   ( fullDesc
  --       <> progDesc "Print a greeting for TARGET"
  --       <> header "hello - a test for optparse-applicative"
  --   )
  args <- Promise.fromIO (execParser opts)
  print "Hello, World!"


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