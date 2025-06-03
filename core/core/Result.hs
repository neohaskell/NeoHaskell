-- | A @Result@ is the result of a computation that may fail. This is a great
-- way to manage errors in Elm, but we when using this package in Haskell we
-- tend to rely on 'Task.Task' a lot too for error handling.
module Result (
  -- * Type and Constructors
  Result (..),

  -- * Mapping
  map,

  -- * Chaining
  andThen,

  -- * Handling Errors
  withDefault,
  toMaybe,
  fromMaybe,
  mapError,
  fromEither,
  isOk,
  isErr,
) where

import Basics
import Mappable qualified
import Maybe (Maybe (..))
import Thenable qualified
import Prelude qualified


-- | A @Result@ is either @Ok@ meaning the computation succeeded, or it is an
-- @Err@ meaning that there was some failure.
data Result error value
  = Ok value
  | Err error
  deriving (Prelude.Show, Eq)


instance Prelude.Functor (Result error) where
  fmap func result =
    case result of
      Ok value -> Ok (func value)
      Err error -> Err error


instance Prelude.Applicative (Result error) where
  pure = Ok


  (<*>) r1 r2 =
    case (r1, r2) of
      (Ok func, Ok a) -> Ok (func a)
      (Err error, _) -> Err error
      (Ok _, Err error) -> Err error


instance Prelude.Monad (Result error) where
  (>>=) result func =
    case result of
      Ok value -> func value
      Err error -> Err error


-- | If the result is @Ok@ return the value, but if the result is an @Err@ then
-- return a given default value. The following examples try to parse integers.
--
-- > Result.withDefault 0 (Ok 123)   == 123
-- > Result.withDefault 0 (Err "no") == 0
withDefault :: a -> Result b a -> a
withDefault fallback result =
  case result of
    Ok value -> value
    Err _ -> fallback


-- | Apply a function to a result. If the result is @Ok@, it will be converted.
-- If the result is an @Err@, the same error value will propagate through.
--
-- > map sqrt (Ok 4.0)          == Ok 2.0
-- > map sqrt (Err "bad input") == Err "bad input"
map :: (a -> value) -> Result x a -> Result x value
map =
  Mappable.map


-- | Chain together a sequence of computations that may fail. It is helpful
-- to see its definition:
--
-- > andThen : (a -> Result e b) -> Result e a -> Result e b
-- > andThen callback result =
-- >     case result of
-- >       Ok value -> callback value
-- >       Err msg -> Err msg
--
-- This means we only continue with the callback if things are going well. For
-- example, say you need to use (@toInt : String -> Result String Int@) to parse
-- a month and make sure it is between 1 and 12:
--
-- > toValidMonth : Int -> Result String Int
-- > toValidMonth month =
-- >     if month >= 1 && month <= 12
-- >         then Ok month
-- >         else Err "months must be between 1 and 12"
--
-- > toMonth : String -> Result String Int
-- > toMonth rawString =
-- >     toInt rawString
-- >       |> andThen toValidMonth
--
-- > -- toMonth "4" == Ok 4
-- > -- toMonth "9" == Ok 9
-- > -- toMonth "a" == Err "cannot parse to an Int"
-- > -- toMonth "0" == Err "months must be between 1 and 12"
--
-- This allows us to come out of a chain of operations with quite a specific error
-- message. It is often best to create a custom type that explicitly represents
-- the exact ways your computation may fail. This way it is easy to handle in your
-- code.
andThen :: (a -> Result c b) -> Result c a -> Result c b
andThen =
  Thenable.andThen


-- | Transform an @Err@ value. For example, say the errors we get have too much
-- information:
--
-- > parseInt : String -> Result ParseError Int
-- >
-- > type alias ParseError =
-- >     { message : String
-- >     , code : Int
-- >     , position : (Int,Int)
-- >     }
-- >
-- > mapError .message (parseInt "123") == Ok 123
-- > mapError .message (parseInt "abc") == Err "char 'a' is not a number"
mapError :: (a -> b) -> Result a c -> Result b c
mapError func result =
  case result of
    Ok value -> Ok value
    Err error -> Err (func error)


-- | Convert to a simpler @Maybe@ if the actual error message is not needed or
-- you need to interact with some code that primarily uses maybes.
--
-- > parseInt : String -> Result ParseError Int
-- >
-- > maybeParseInt : String -> Maybe Int
-- > maybeParseInt string =
-- >     toMaybe (parseInt string)
toMaybe :: Result a b -> Maybe b
toMaybe result =
  case result of
    Ok value -> Just value
    Err _ -> Nothing


-- | Convert from a simple @Maybe@ to interact with some code that primarily
-- uses @Results@.
--
-- > parseInt : String -> Maybe Int
-- >
-- > resultParseInt : String -> Result String Int
-- > resultParseInt string =
-- >     fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
fromMaybe :: a -> Maybe b -> Result a b
fromMaybe error maybe =
  case maybe of
    Just something -> Ok something
    Nothing -> Err error


-- | Compatibility function to integrate with Haskell's @Either@ type.
--
-- > fromEither (Prelude.Right 42) == Ok 42
-- > fromEither (Prelude.Left "nope") == Err "nope"
fromEither :: Prelude.Either a b -> Result a b
fromEither either =
  case either of
    Prelude.Left a -> Err a
    Prelude.Right b -> Ok b


-- | Check if a Result is Ok. Returns True if the Result is Ok, False if it is Err.
--
-- > isOk (Ok 42) == True
-- > isOk (Err "nope") == False
isOk :: Result a b -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False


-- | Check if a Result is Err. Returns True if the Result is Err, False if it is Ok.
--
-- > isErr (Ok 42) == False
-- > isErr (Err "nope") == True
isErr :: Result a b -> Bool
isErr result =
  case result of
    Ok _ -> False
    Err _ -> True
