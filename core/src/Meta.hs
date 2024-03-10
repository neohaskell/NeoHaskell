module Meta (
  TypeString,
  DeclarationName,
  Macro,
  Declaration,
  DeclarationMacro,
  typed,
  TypeReference,
) where

import GHC.TypeLits qualified as GHC
import Language.Haskell.TH.Syntax qualified as TH


type TypeString = GHC.Symbol


type DeclarationName = TH.Name


type Macro = TH.Q


type Declaration = TH.Dec


type DeclarationMacro = Macro [Declaration]


data TypeReference someType = TypeReference


typed :: forall someType. TypeReference someType
typed = TypeReference