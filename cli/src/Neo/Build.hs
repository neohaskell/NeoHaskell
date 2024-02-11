module Neo.Build where

import Core
import Options.Generic qualified as Opts

type (|>) (a :: Type) (f :: Type -> Type) = f a

type ArgsDefault a b = (Opts.<!>) b a
type ArgsHelp a b = (Opts.<?>) b a
type ArgsShort a b = (Opts.<#>) b a

data Args = Args
  { name :: String
            |> ArgsDefault "world"
            |> ArgsHelp "Name of the person to greet"
            |> ArgsShort "n"
  } deriving (Generic, Show)

instance Opts.ParseRecord Args

start :: Promise Void
start = do
  x <- Opts.getRecord "neo"
  print (x :: Args)