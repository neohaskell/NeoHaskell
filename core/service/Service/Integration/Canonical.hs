-- | First-party RFC 8785 canonical-JSON encoder and SHA-256 hasher.
--
-- Implementation notes:
--
-- * Object keys are sorted by UTF-16 code-unit order (RFC 8785 §3.2.3).
-- * Numbers use shortest round-trippable 'Scientific' formatting with
--   ECMA-262 §7.1.12.1 style.
-- * 'NaN' and '±Infinity' are rejected with 'ValidationFailure'.
-- * Strings escape only @"@, @\\@, and @U+0000..U+001F@ per RFC 8785 §3.2.2.2.
module Service.Integration.Canonical
  ( encode,
    hash,
    version,
    encodeValue,
    hashValue,
  )
where

import Basics
import Crypto.Hash qualified as Hash
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteArray.Encoding qualified as ByteArrayEncoding
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char (Char)
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
import Data.Scientific qualified as Scientific
import Data.Text qualified as GhcText
import Data.Text.Encoding qualified as GhcTextEncoding
import Data.Vector qualified as Vector
import Prelude (Double, (<>), mempty, (++), (/=), div, mod)
import Prelude qualified
import Result (Result (..))
import Service.Integration.Canonical.Version qualified as CanonicalVersion
import Service.Integration.IntegrationError (IntegrationError (..))
import Text (Text)


-- | The canonical-JSON format version. See ADR-0055 §12.
version :: Int
version = CanonicalVersion.version


-- | Encode a 'Aeson.ToJSON' value into canonical JSON bytes.
--
-- Returns 'Err' when the value contains 'NaN' or '±Infinity'.
encode :: (Aeson.ToJSON value) => value -> Result IntegrationError ByteString
encode value =
  encodeValue (Aeson.toJSON value)


-- | Hash a 'Aeson.ToJSON' value as lowercase hex SHA-256 of its canonical bytes.
hash :: (Aeson.ToJSON value) => value -> Result IntegrationError Text
hash value = do
  case encode value of
    Err err -> Err err
    Ok bytes -> Ok (hashBytes bytes)


-- | Hash already-canonical bytes to lowercase hex SHA-256.
hashBytes :: ByteString -> Text
hashBytes bytes = do
  let digest = Hash.hash bytes :: Hash.Digest Hash.SHA256
  let hex = ByteArrayEncoding.convertToBase ByteArrayEncoding.Base16 digest :: ByteString
  GhcTextEncoding.decodeUtf8 hex


-- | Public pure-'Aeson.Value' variant — used by tests that already hold a
-- 'Value' tree (RFC 8785 vectors).
encodeValue :: Aeson.Value -> Result IntegrationError ByteString
encodeValue value =
  case encodeV value of
    Err err -> Err err
    Ok builder -> Ok (LBS.toStrict (Builder.toLazyByteString builder))


-- | Hash a pure 'Aeson.Value' as lowercase hex SHA-256.
hashValue :: Aeson.Value -> Result IntegrationError Text
hashValue value = do
  case encodeValue value of
    Err err -> Err err
    Ok bytes -> Ok (hashBytes bytes)


-- | Internal core: turns an 'Aeson.Value' into a canonical 'Builder.Builder'.
encodeV :: Aeson.Value -> Result IntegrationError Builder.Builder
encodeV value =
  case value of
    Aeson.Null -> Ok (Builder.byteString "null")
    Aeson.Bool True -> Ok (Builder.byteString "true")
    Aeson.Bool False -> Ok (Builder.byteString "false")
    Aeson.String text -> Ok (encodeString text)
    Aeson.Number num -> encodeNumber num
    Aeson.Array vec -> encodeArray vec
    Aeson.Object obj -> encodeObject obj


-- Arrays -------------------------------------------------------------------

encodeArray :: Vector.Vector Aeson.Value -> Result IntegrationError Builder.Builder
encodeArray vec = do
  let items = Vector.toList vec
  encoded <- mapResult encodeV items
  let commaSep = interleaveCommas encoded
  Ok (Builder.char7 '[' <> commaSep <> Builder.char7 ']')


-- Objects ------------------------------------------------------------------

encodeObject ::
  AesonKeyMap.KeyMap Aeson.Value ->
  Result IntegrationError Builder.Builder
encodeObject obj = do
  let entries = AesonKeyMap.toList obj
  let pairs = GhcList.map (\(key, val) -> (AesonKey.toText key, val)) entries
  -- UTF-16LE sort key so multi-byte chars get BMP-equivalent comparison
  let sorted = GhcList.sortOn (\(k, _) -> utf16Key k) pairs
  encodedPairs <- mapResult encodePair sorted
  let commaSep = interleaveCommas encodedPairs
  Ok (Builder.char7 '{' <> commaSep <> Builder.char7 '}')
  where
    encodePair (key, val) = do
      valEncoded <- encodeV val
      Ok (encodeString key <> Builder.char7 ':' <> valEncoded)


-- | Derive the UTF-16LE byte sequence of a Text for sort purposes.
utf16Key :: Text -> ByteString
utf16Key = GhcTextEncoding.encodeUtf16LE


-- Strings ------------------------------------------------------------------

-- | Serialize a 'Text' as an RFC 8785 canonical JSON string literal.
encodeString :: Text -> Builder.Builder
encodeString text =
  Builder.char7 '"' <> escapeString text <> Builder.char7 '"'


-- | RFC 8785 §3.2.2.2 string escape rules.
escapeString :: Text -> Builder.Builder
escapeString text =
  GhcText.foldr appendChar mempty text
  where
    appendChar c builder = escapeChar c <> builder


escapeChar :: Char -> Builder.Builder
escapeChar c = case c of
  '"' -> Builder.byteString "\\\""
  '\\' -> Builder.byteString "\\\\"
  '\b' -> Builder.byteString "\\b"
  '\f' -> Builder.byteString "\\f"
  '\n' -> Builder.byteString "\\n"
  '\r' -> Builder.byteString "\\r"
  '\t' -> Builder.byteString "\\t"
  ch
    | GhcChar.ord ch < 0x20 -> controlEscape (GhcChar.ord ch)
    | otherwise -> Builder.stringUtf8 [ch]


controlEscape :: Int -> Builder.Builder
controlEscape i = do
  let hex = "0123456789abcdef" :: [Char]
  let nybble n = hex GhcList.!! (n `mod` 16)
  let d3 = nybble (i `div` 4096)
  let d2 = nybble (i `div` 256)
  let d1 = nybble (i `div` 16)
  let d0 = nybble i
  Builder.stringUtf8 ['\\', 'u', d3, d2, d1, d0]


-- Numbers ------------------------------------------------------------------

-- | Detect 'Aeson.Number' values whose 'Scientific' representation encodes
-- @NaN@ or @±Infinity@. Aeson doesn't currently produce such values, but we
-- keep a best-effort check so that user-supplied 'Aeson.Number' constructions
-- via alternate paths are rejected per ADR-0055 §12.
encodeNumber :: Scientific.Scientific -> Result IntegrationError Builder.Builder
encodeNumber sci
  | isNaNLike sci = Err (ValidationFailure "NaN not permitted in canonical JSON")
  | isInfiniteLike sci = Err (ValidationFailure "Infinity not permitted in canonical JSON")
  | otherwise = Ok (formatScientific sci)


isNaNLike :: Scientific.Scientific -> Bool
isNaNLike _ = False  -- Scientific cannot represent NaN by construction


isInfiniteLike :: Scientific.Scientific -> Bool
isInfiniteLike _ = False  -- Scientific cannot represent infinity by construction


-- | Render a 'Scientific' number per ECMA-262 §7.1.12.1 / RFC 8785 §3.2.2.3.
--
-- Uses the shortest round-trippable decimal. Magnitudes outside
-- @[1e-6, 1e21)@ switch to exponential form with a lowercase @e@ and explicit
-- sign.
formatScientific :: Scientific.Scientific -> Builder.Builder
formatScientific sci
  | Scientific.coefficient sci == 0 = Builder.byteString "0"
  | otherwise = do
      let normalized = Scientific.normalize sci
      let asDouble = Scientific.toRealFloat normalized :: Double
      let magnitude = Prelude.abs asDouble
      if magnitude /= 0 && (magnitude >= 1e21 || magnitude < 1e-6)
        then formatExponential normalized
        else formatPlain normalized


formatPlain :: Scientific.Scientific -> Builder.Builder
formatPlain sci = do
  let asDouble = Scientific.toRealFloat sci :: Double
  let rendered = shortestDecimal asDouble
  Builder.stringUtf8 rendered


formatExponential :: Scientific.Scientific -> Builder.Builder
formatExponential sci = do
  let asDouble = Scientific.toRealFloat sci :: Double
  let rendered = shortestExponential asDouble
  Builder.stringUtf8 rendered


-- | Produce the shortest decimal representation of a 'Double' that
-- round-trips.
shortestDecimal :: Double -> [Char]
shortestDecimal d = do
  let shown = show d
  -- Haskell's Show on Double produces "1.0" for 1; strip trailing ".0".
  case dropWhileEnd (== '0') (stripTrailingDotZero shown) of
    [] -> "0"
    result -> stripTrailingDot result


stripTrailingDotZero :: [Char] -> [Char]
stripTrailingDotZero s =
  if ".0" `GhcList.isSuffixOf` s
    then GhcList.take (GhcList.length s - 2) s
    else s


stripTrailingDot :: [Char] -> [Char]
stripTrailingDot s =
  if "." `GhcList.isSuffixOf` s
    then GhcList.take (GhcList.length s - 1) s
    else s


shortestExponential :: Double -> [Char]
shortestExponential d = do
  -- Use Haskell's default Show and rewrite the exponent format to ECMA-262.
  -- Show prints as "1.0e21" -> "1e+21"
  let shown = show d
  case GhcList.break (== 'e') shown of
    (mantissa, 'e' : expPart) -> do
      let cleanMantissa = stripTrailingDotZero mantissa
      let sign = case expPart of
            '-' : _ -> ""
            '+' : _ -> ""
            _ -> "+"
      cleanMantissa ++ "e" ++ sign ++ expPart
    _ -> shown


dropWhileEnd :: (Char -> Bool) -> [Char] -> [Char]
dropWhileEnd predicate s = GhcList.reverse (GhcList.dropWhile predicate (GhcList.reverse s))


-- Helpers ------------------------------------------------------------------

interleaveCommas :: [Builder.Builder] -> Builder.Builder
interleaveCommas xs = case xs of
  [] -> mempty
  [x] -> x
  x : rest -> x <> Builder.char7 ',' <> interleaveCommas rest


mapResult :: (a -> Result err b) -> [a] -> Result err [b]
mapResult f xs = case xs of
  [] -> Ok []
  y : rest -> case f y of
    Err e -> Err e
    Ok b -> case mapResult f rest of
      Err e -> Err e
      Ok bs -> Ok (b : bs)


