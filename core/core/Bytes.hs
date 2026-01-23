module Bytes (
  Bytes (..),
  unwrap,
  fromLegacy,
  toLazyLegacy,
  fromLazyLegacy,
  length,
  empty,
  replicate,
  pack,
  -- * Searching
  findSubstring,
  splitOn,
  splitOnce,
  -- * Slicing
  take,
  drop,
  takeEnd,
  dropEnd,
  -- * Predicates
  isPrefixOf,
  isSuffixOf,
  -- * Combining
  append,
  concat,
) where

import Basics
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Search qualified as ByteStringSearch
import Data.Word (Word8)
import Prelude qualified as GhcPrelude


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString
  deriving (Eq, Show, Ord, Generic, IsString)


unwrap :: Bytes -> ByteString.ByteString
unwrap (INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = bytes


fromLegacy :: ByteString.ByteString -> Bytes
fromLegacy = INTERNAL_CORE_BYTES_CONSTRUCTOR


fromLazyLegacy :: LazyByteString -> Bytes
fromLazyLegacy lbs =
  lbs
    |> ByteString.toStrict
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


toLazyLegacy :: Bytes -> LazyByteString
toLazyLegacy bytes =
  bytes
    |> unwrap
    |> ByteString.fromStrict


length :: Bytes -> GhcPrelude.Int
length (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.length bs


empty :: Bytes
empty = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.empty


replicate :: GhcPrelude.Int -> Word8 -> Bytes
replicate n byte =
  ByteString.replicate n byte
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


pack :: [Word8] -> Bytes
pack ws =
  ByteString.pack ws
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Find the first occurrence of a pattern in bytes.
-- Returns the index of the first byte of the pattern, or Nothing if not found.
findSubstring :: Bytes -> Bytes -> GhcPrelude.Maybe GhcPrelude.Int
findSubstring (INTERNAL_CORE_BYTES_CONSTRUCTOR needle) (INTERNAL_CORE_BYTES_CONSTRUCTOR haystack) =
  case ByteStringSearch.indices needle haystack of
    [] -> GhcPrelude.Nothing
    (i : _) -> GhcPrelude.Just i


-- | Split bytes on every occurrence of the delimiter.
-- The delimiter is not included in any of the results.
--
-- WARNING: If the delimiter is empty, this returns an infinite list.
-- Callers MUST validate that the delimiter is non-empty before calling.
splitOn :: Bytes -> Bytes -> [Bytes]
splitOn (INTERNAL_CORE_BYTES_CONSTRUCTOR delim) (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteStringSearch.split delim bs
    |> GhcPrelude.map INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Split bytes on the first occurrence of the delimiter.
-- Returns (before, after) where the delimiter is not included in either part.
-- Returns Nothing if the delimiter is not found.
splitOnce :: Bytes -> Bytes -> GhcPrelude.Maybe (Bytes, Bytes)
splitOnce needle haystack =
  case findSubstring needle haystack of
    GhcPrelude.Nothing -> GhcPrelude.Nothing
    GhcPrelude.Just idx -> do
      let before = take idx haystack
      let after = drop (idx GhcPrelude.+ length needle) haystack
      GhcPrelude.Just (before, after)


-- | Take the first n bytes.
take :: GhcPrelude.Int -> Bytes -> Bytes
take n (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.take n bs
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Drop the first n bytes.
drop :: GhcPrelude.Int -> Bytes -> Bytes
drop n (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.drop n bs
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Take the last n bytes.
takeEnd :: GhcPrelude.Int -> Bytes -> Bytes
takeEnd n (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.takeEnd n bs
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Drop the last n bytes.
dropEnd :: GhcPrelude.Int -> Bytes -> Bytes
dropEnd n (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.dropEnd n bs
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Check if the first bytes is a prefix of the second.
isPrefixOf :: Bytes -> Bytes -> GhcPrelude.Bool
isPrefixOf (INTERNAL_CORE_BYTES_CONSTRUCTOR prefix) (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.isPrefixOf prefix bs


-- | Check if the first bytes is a suffix of the second.
isSuffixOf :: Bytes -> Bytes -> GhcPrelude.Bool
isSuffixOf (INTERNAL_CORE_BYTES_CONSTRUCTOR suffix) (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.isSuffixOf suffix bs


-- | Append two byte sequences.
append :: Bytes -> Bytes -> Bytes
append (INTERNAL_CORE_BYTES_CONSTRUCTOR a) (INTERNAL_CORE_BYTES_CONSTRUCTOR b) =
  ByteString.append a b
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


-- | Concatenate a list of byte sequences.
concat :: [Bytes] -> Bytes
concat bss =
  bss
    |> GhcPrelude.map unwrap
    |> ByteString.concat
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR