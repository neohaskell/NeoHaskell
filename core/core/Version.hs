{-# OPTIONS_GHC -Wno-orphans #-}

module Version (Version, version, parse
    , toText) where

import Array qualified
import Basics
import Char (Char)
import Control.Monad.Fail qualified as GHC
import Data.List qualified as GHC
import Data.Version (Version (..))
import Data.Version qualified
import GHC.Real qualified as GHC
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Quote qualified as Quote
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList (LinkedList)
import Mappable qualified
import Maybe (Maybe (..))
import Text (Text)
import Text qualified
import Text.ParserCombinators.ReadP qualified as ReadP
import ToText qualified


instance TH.Lift Version where
  lift (Version branch tags) =
    TH.appE
      (TH.conE 'Version)
      ( branch
          |> Mappable.map (GHC.fromIntegral .> TH.integerL)
          |> Mappable.map TH.litE
          |> TH.listE
      )
      `TH.appE` ( tags
                    |> Mappable.map TH.stringL
                    |> Mappable.map TH.litE
                    |> TH.listE
                )
  liftTyped (Version branch tags) =
    TH.unsafeCodeCoerce (TH.lift (Version branch tags))


-- | Smart text constructor for versions.
--
-- Example:
--
-- @
-- let ver = [version|0.0.0-alpha|]
-- @
--
-- This will create a version with branch [0, 0, 0] and tags ["alpha"].
version :: Quote.QuasiQuoter
version =
  QuasiQuoter
    { quoteExp = parseVersionExp,
      quotePat = \_ -> GHC.fail "Versions can only be used as expressions",
      quoteType = \_ -> GHC.fail "Versions can only be used as expressions",
      quoteDec = \_ -> GHC.fail "Versions can only be used as expressions"
    }


-- | Parse a version from a Text.
--
-- Example:
--
-- > Version.parse "0.0.0-alpha"
-- Just (Version [0, 0, 0] ["alpha"])
parse :: Text -> Maybe Version
parse input =
  case ReadP.readP_to_S Data.Version.parseVersion (Text.toLinkedList input) of
    [] -> Nothing
    parses ->
      case GHC.find (\(_, remaining) -> remaining == "") parses of
        Just (v, "") -> Just v
        _ -> Nothing


parseVersionExp :: LinkedList Char -> TH.Q TH.Exp
parseVersionExp input =
  case ReadP.readP_to_S Data.Version.parseVersion input of
    [] -> GHC.fail "Invalid version format: no valid parse"
    parses ->
      case GHC.find (\(_, remaining) -> remaining == "") parses of
        Just (v, "") -> [|v|]
        _ -> GHC.fail "Invalid version format, must be in the form A.B.C-XYZ"

toText :: Version -> Text
toText (Version branch tags) = do
  let textTags =
        Array.fromLinkedList tags
          |> Array.map Text.fromLinkedList
          |> Text.joinWith "-"
  let textBranch =
        Array.fromLinkedList branch
          |> Array.map ToText.toText
          |> Text.joinWith "."
  Array.fromLinkedList [textBranch, textTags]
    |> Array.dropIf Text.isEmpty
    |> Text.joinWith "-"