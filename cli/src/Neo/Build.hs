{-# LANGUAGE RecordWildCards #-}

module Neo.Build where

import Accumulator qualified
import Array qualified
import Core
import Data.Functor qualified as Functor
import Data.Monoid qualified as Monoid
import Data.String qualified as Ghc
import Data.Traversable qualified as Traversable
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.List qualified as LegacyList
import Options.Applicative
import Promise qualified
import Reflect qualified
import Traits.Dsl
import Traits.Schema
import Traits.Schema qualified as Schema
import Prelude qualified as Ghc


data Args = Args
  { name :: String
  }
  deriving (Reflect.TypeInfo)

record' #Person do
  description "A person"
  property @"name" do
    description "The name of the person"
    shorthand 'n'
  property @"age" do
    description "The age of the person"
    shorthand 'a'


schema :: Schema Args
schema = record do
  property @"name" definition do
    description "Name to greet in the application"
    shorthand 'n'


instance Schematized Args where
  schema_impl = schema


start :: Promise Void
start = do
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