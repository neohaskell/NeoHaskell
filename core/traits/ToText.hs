{-# LANGUAGE UndecidableInstances #-}

module ToText (Show (..), toText, ToText) where

import Basics
import Data.Text.Lazy qualified as LazyText
import Text (Text)
import Text.Pretty.Simple qualified as PS
import Prelude (Show (..))


type ToText value = Show value


toText :: (Show value) => value -> Text
toText value =
  PS.pShow value
    |> LazyText.toStrict