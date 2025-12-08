{-# LANGUAGE UndecidableInstances #-}

module ToText (
  Show (..),
  toPrettyText,
  ToText,
  toText,
) where

import Basics
import Data.Text.Lazy qualified as LazyText
import Text (Text, fromLinkedList)
import Text.Pretty.Simple qualified as PS


type ToText value = Show value


toPrettyText :: (Show value) => value -> Text
toPrettyText value =
  PS.pShow value
    |> LazyText.toStrict


toText :: (Show value) => value -> Text
toText value =
  show value
    |> Text.fromLinkedList
