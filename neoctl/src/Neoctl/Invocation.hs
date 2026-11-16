{-| Represent the supported startup choices for neoctl.
Use 'classifyArguments' to accept a complete argument vector or reject it.
-}
module Neoctl.Invocation (
  Invocation (..),
  classifyArguments,
) where

import Core


data Invocation
  = ShowHelp
  | ShowVersion
  | InvalidArguments


classifyArguments :: [Text] -> Invocation
classifyArguments arguments = case arguments of
  [] -> ShowHelp
  ["--help"] -> ShowHelp
  ["-h"] -> ShowHelp
  ["--version"] -> ShowVersion
  _ -> InvalidArguments
