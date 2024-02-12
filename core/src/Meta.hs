module Meta (
  TypeString,
  DeclarationName,
  CodeGeneration,
  Declaration,
) where

import GHC.TypeLits qualified as GHC
import Language.Haskell.TH.Syntax qualified as TH
import Types (Array)


type TypeString = GHC.Symbol


type DeclarationName = TH.Name


type CodeGeneration = TH.Q


type Declaration = TH.Dec
