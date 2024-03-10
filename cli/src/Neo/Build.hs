{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Neo.Build where

import Core
import Data.Functor qualified as Functor
import Data.Monoid qualified as Monoid
import Data.Typeable qualified as Ghc
import HaskellCompatibility.Conversion qualified as Convert
import Language.Haskell.TH qualified as TH
import Options.Applicative
import Options.Applicative qualified as OptParse
import Options.Applicative.Types qualified as OptParse
import Promise qualified
import Schema qualified
import Schema.Types
import Unsafe.Coerce (unsafeCoerce)


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


toOptParser :: Schema a -> Parser a
toOptParser s = case s of
  Schema.NullSchema o -> OptParse.NilP (Convert.toLegacy o)
  Schema.PropertySchema options -> (propToOptParser options)
  Schema.PartialSchema f innerSchema -> OptParse.MultP (toOptParser f) (toOptParser innerSchema)
  Schema.AlternativeSchema left right -> OptParse.AltP (toOptParser left) (toOptParser right)
  Schema.ContinuationSchema innerSchema f -> OptParse.BindP (toOptParser innerSchema) (f .> toOptParser)



propToOptParser :: forall a. PropertyOptions a -> Parser a
propToOptParser options = do
  let parser = $(foo options)
  parser
    ( long (options.name |> Convert.toLegacy)
        Monoid.<> metavar (options.placeholder |> Convert.toLegacy)
        Monoid.<> help (options.description |> Convert.toLegacy)
        -- Monoid.<> showDefault
        Monoid.<> value (options.defaultsTo)
        Monoid.<> (if options.hidden then OptParse.hidden else Monoid.mempty)
    )

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