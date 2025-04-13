{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeName (
  Inspectable,
  get,
  reflect,
) where

import Basics
import Data.Typeable qualified as Typeable
import GHC.Base qualified as GhcBase
import Text (Text)
import Text qualified
import ToText (toText)


type Inspectable a = Typeable.Typeable a


-- | Get the type name of a value.
get :: (Inspectable a) => a -> Text
get value =
  Typeable.typeOf value
    |> toText
    -- We can add replacements here
    |> replaceTypes


reflect :: forall (a :: Type). (Inspectable a) => Text
reflect =
  Typeable.typeOf (GhcBase.undefined :: a)
    |> toText
    |> replaceTypes


replaceTypes :: Text -> Text
replaceTypes =
  -- TODO: Add more replacements like [Int] -> LinkedList Int
  Text.replace "Data.Text.Internal" "Text"
