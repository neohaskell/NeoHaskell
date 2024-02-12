module Pipe (
  (|>),
  (<|),
  (.>),
  (<.),
  type (|>),
) where

import Data.Function ((.))
import Kinds (Type)


-- | The forward pipe operator.
--
-- It is used to pipe a value into a function.
-- You can think of it as the pipe operator in
-- Bash. It acts exactly the same as the pipe
-- operator in F#, Elixir, Elm, and others.
--
-- >>> 1 |> (+) 2 |> (*) 3
-- 9
(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
{-# INLINE (|>) #-}


-- We tell the compiler that the precedence of
-- the pipe operator is 0, so that it is
-- evaluated last. Also, we tell the compiler
-- that the pipe operator is left associative,
-- which means that the following two
-- expressions are equivalent:
--
--   a |> f |> g
--   (a |> f) |> g
infixl 0 |>


-- | The backward pipe operator.
--
-- You can think of it as wrapping the expression
-- in parentheses, from the point where the pipe
-- operator is used, to the end of the expression.
--
-- >>> (+) 1 <| 2 + 3
-- 6
(<|) :: (a -> b) -> a -> b
(<|) f a =
  f a
{-# INLINE (<|) #-}


-- We tell the compiler that the precedence of
-- the backward pipe operator is 0, so that it
-- is evaluated last. Also, we tell the compiler
-- that the backward pipe operator is right
-- associative, which means that the following
-- two expressions are equivalent:
--
--   f <| a <| b
--   f <| (a <| b)
infixr 0 <|


-- | The forward pipe function composition operator,
-- which allows composing functions in a forward
-- pipe fashion.
--
-- >>> (not .> Array.empty) [1]
-- True
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g =
  g . f
{-# INLINE (.>) #-}


-- | The backward pipe function composition operator,
-- which allows composing functions in a backward
-- pipe fashion.
--
-- >>> (Array.empty <. not) [1]
-- True
(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) f g =
  f . g
{-# INLINE (<.) #-}


-- | The forward pipe type operator for type-level operations.
--
-- It is used to pipe types into other types
type family (|>) (a :: Type) (f :: Type -> Type) :: Type where
  (|>) a f = f a