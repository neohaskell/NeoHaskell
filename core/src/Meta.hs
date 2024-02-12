module Meta (
  TypeString,
  DeclarationName,
  Macro,
  Declaration,
  DeclarationMacro,
) where

import GHC.TypeLits qualified as GHC
import Language.Haskell.TH.Syntax qualified as TH


type TypeString = GHC.Symbol


type DeclarationName = TH.Name


type Macro = TH.Q


type Declaration = TH.Dec


type DeclarationMacro = Macro [Declaration]