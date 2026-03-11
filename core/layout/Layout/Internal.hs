module Layout.Internal (
  Blueprint (..),
  RenderOptions (..),
  WrapMode (..),
  ToBlueprint (..),
  wrap,
  unwrap,
) where

import Basics
import Data.Functor (Functor)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Maybe (Maybe (..))
import Prettyprinter qualified as GhcPretty
import Result (Result (..))
import Text (Text)
import Text qualified


newtype Blueprint annotation
  = Blueprint (GhcPretty.Doc annotation)
  deriving (Functor)


data RenderOptions = RenderOptions
  { maxWidth :: {-# UNPACK #-} Int
  , ribbonFraction :: {-# UNPACK #-} Float
  , wrapMode :: WrapMode
  }


data WrapMode
  = Balanced
  | Fast
  | Compact
  deriving (Eq, Show, Generic)


class ToBlueprint value where
  toBlueprint :: forall annotation. value -> Blueprint annotation


wrap :: GhcPretty.Doc annotation -> Blueprint annotation
wrap = Blueprint
{-# INLINE wrap #-}


unwrap :: Blueprint annotation -> GhcPretty.Doc annotation
unwrap (Blueprint doc) = doc
{-# INLINE unwrap #-}


instance Semigroup (Blueprint annotation) where
  left <> right = wrap (unwrap left <> unwrap right)


instance Monoid (Blueprint annotation) where
  mempty = wrap GhcPretty.emptyDoc


instance IsString (Blueprint annotation) where
  fromString s = wrap (GhcPretty.pretty (Text.fromLinkedList s))


instance ToBlueprint Text where
  toBlueprint textValue = wrap (GhcPretty.pretty textValue)


instance ToBlueprint Int where
  toBlueprint intValue = wrap (GhcPretty.pretty intValue)


instance ToBlueprint Float where
  toBlueprint floatValue = wrap (GhcPretty.pretty floatValue)


instance ToBlueprint Bool where
  toBlueprint boolValue =
    case boolValue of
      True -> wrap (GhcPretty.pretty ("True" :: Text))
      False -> wrap (GhcPretty.pretty ("False" :: Text))


instance (ToBlueprint value) => ToBlueprint (Maybe value) where
  toBlueprint maybeValue =
    case maybeValue of
      Nothing -> wrap (GhcPretty.pretty ("Nothing" :: Text))
      Just v ->
        wrap
          ( GhcPretty.pretty ("Just " :: Text)
              GhcPretty.<> unwrap (toBlueprint v)
          )


instance (ToBlueprint error, ToBlueprint value) => ToBlueprint (Result error value) where
  toBlueprint resultValue =
    case resultValue of
      Err e ->
        wrap
          ( GhcPretty.pretty ("Err " :: Text)
              GhcPretty.<> unwrap (toBlueprint e)
          )
      Ok v ->
        wrap
          ( GhcPretty.pretty ("Ok " :: Text)
              GhcPretty.<> unwrap (toBlueprint v)
          )
