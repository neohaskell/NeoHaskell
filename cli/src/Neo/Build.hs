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


schema :: Schema Args
schema = record do
  property @"name" definition do
    description "Name to greet in the application"
    shorthand "n"


instance Schematized Args where
  schema_impl = schema


argsParser :: Parser Args
argsParser = do
  let schemaFoo = schema |> Schema.getDefinition
  case schemaFoo of
    Schema.RecordSchemaDefinition recordSchema -> makeParser recordSchema
    _ -> todo


makeParser :: RecordSchema -> Parser Args
makeParser recordSchema =
  (recordSchema.recordProperties ?? [])
    |> Array.applyToEach makePropertyParser
    |> LegacyList.toList
    |> Functor.fmap (Functor.fmap _)


longNH :: String -> Mod OptionFields a
longNH name = Convert.toLegacy name |> long


helpNH :: String -> Mod OptionFields a
helpNH descTxt = Convert.toLegacy descTxt |> help


makePropertyParser :: (Ghc.IsString a, Ghc.Read a) => PropertySchema -> Parser a
makePropertyParser propertySchema =
  do
    let propName = propertySchema.propertyName ?? ""
    let longOption = longNH (propName)
    let shortOption = short (propertySchema.propertyShorthand ?? 'A')
    let helpOption = helpNH (propertySchema.propertyDescription ?? "NO HELP")
    let optParser =
          case propertySchema.propertySchema ?? todo of
            Schema.PrimitiveSchemaDefinition primitiveSchema -> do
              case primitiveSchema of
                Schema.StringSchema -> strOption
                Schema.IntSchema -> option auto
                _ -> todo
            _ -> todo
    [longOption, shortOption, helpOption]
      |> LegacyList.toList
      |> Monoid.mconcat
      |> optParser


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