-- | This library fills a bunch of important niches in Elm. A Maybe can help you with optional arguments, error handling, and records with optional fields.
module Maybe
  ( -- * Definition
    Maybe (..),

    -- * Common Helpers
    withDefault,
    map,

    -- * Chaining Maybes
    andThen,
    getOrDie,
  )
where

import Basics
import Data.Maybe (Maybe (..), fromMaybe)
import Mappable qualified
import Thenable qualified

-- | Provide a default value, turning an optional value into a normal
-- value.  This comes in handy when paired with functions like
-- 'Dict.get' which gives back a @Maybe@.
--
-- __Note:__ This can be overused! Many cases are better handled by a @case@
-- expression. And if you end up using @withDefault@ a lot, it can be a good sign
-- that a [custom type](https://guide.elm-lang.org/types/custom_types.html)
-- will clean your code up quite a bit!
withDefault :: a -> Maybe a -> a
withDefault =
  Data.Maybe.fromMaybe

-- | Transform a @Maybe@ value with a given function:
map :: (a -> b) -> Maybe a -> Maybe b
map =
  Mappable.map

-- | Chain together many computations that may fail. It is helpful to see an
-- equivalent definition:
--
-- > andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
-- > andThen callback maybe =
-- >   case maybe of
-- >     Just value ->
-- >       callback value
-- >
-- >     Nothing ->
-- >       Nothing
--
-- This means we only continue with the callback if things are going well. For
-- example, say you need to parse some user input as a month:
--
-- > parseMonth :: String -> Maybe Int
-- > parseMonth userInput =
-- >   String.toInt userInput
-- >     |> andThen toValidMonth
-- >
-- > toValidMonth :: Int -> Maybe Int
-- > toValidMonth month =
-- >   if 1 <= month && month <= 12 then
-- >     Just month
-- >
-- >   else
-- >     Nothing
--
-- In the @parseMonth' function, if 'String.toInt@ produces @Nothing@ (because
-- the @userInput@ was not an integer) this entire chain of operations will
-- short-circuit and result in @Nothing@. If @toValidMonth@ results in
-- @Nothing@, again the chain of computations will result in @Nothing@.
andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen =
  Thenable.andThen

-- | Attempts to retrieve the value from a @Maybe@. If the @Maybe@ is @Nothing@,
-- the application will crash abruptly.
getOrDie :: Maybe a -> a
getOrDie maybe =
  case maybe of
    Just value ->
      value
    Nothing ->
      dieWith "Maybe.getOrDie: Got Nothing"
{-# INLINE getOrDie #-}