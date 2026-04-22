-- | Entropy + known-prefix secret scanner used by 'Test.Integration.Fixture'.
module Test.Integration.EntropyScan
  ( scanForSecrets,
    secretPrefixes,
    shannonEntropy,
  )
where

import Appendable ((++))
import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
import Data.Map.Strict qualified as GhcMap
import Data.Vector qualified as Vector
import Result (Result (..))
import Text (Text)
import Text qualified
import Prelude (Double)
import Prelude qualified


-- | Secret-shaped prefixes we always reject.
secretPrefixes :: [Text]
secretPrefixes =
  [ "sk_live_",
    "sk_test_",
    "pk_live_",
    "pk_test_",
    "Bearer ",
    "whsec_",
    "rk_live_",
    "rk_test_",
    "eyJ"
  ]


-- | Scan a JSON value for secrets. Returns 'Err' with a list of reason
-- strings if anything suspicious is found, 'Ok' otherwise.
--
-- The @allowed@ list is a list of JSON-path expressions that exempt a
-- particular field from the scan. This is the 'RedactionRule.AllowEntropyPath'
-- escape hatch.
scanForSecrets :: [Text] -> Aeson.Value -> Result [Text] Aeson.Value
scanForSecrets allowed value = do
  let findings = scanValue allowed "$" value []
  case findings of
    [] -> Ok value
    problems -> Err problems


scanValue :: [Text] -> Text -> Aeson.Value -> [Text] -> [Text]
scanValue allowed jpath val acc =
  if jpath `GhcList.elem` allowed
    then acc
    else case val of
      Aeson.Null -> acc
      Aeson.Bool _ -> acc
      Aeson.Number _ -> acc
      Aeson.String text -> scanString jpath text ++ acc
      Aeson.Array vec ->
        let indexed = Vector.toList (Vector.imap (,) vec)
         in GhcList.foldl'
              (\a (i, v) -> scanValue allowed (jpath ++ "[" ++ Text.fromLinkedList (show i) ++ "]") v a)
              acc
              indexed
      Aeson.Object obj ->
        let entries = AesonKeyMap.toList obj
         in GhcList.foldl'
              ( \a (k, v) ->
                  scanValue allowed (jpath ++ "." ++ AesonKey.toText k) v a
              )
              acc
              entries


-- | Inspect a single string value for secret-shaped patterns. Prefix
-- matching runs as a substring scan so that secrets embedded in free-text
-- fields (e.g. @"auth: Bearer sk_..."@) are still caught.
scanString :: Text -> Text -> [Text]
scanString jpath text = do
  let substringMatches =
        secretPrefixes
          |> GhcList.filter (\p -> Text.contains p text)
  let substringFindings =
        substringMatches
          |> GhcList.map (\p -> [fmt|entropy: #{jpath} contains secret marker #{p}|])
  let longHexFinding =
        if looksLikeLongHex text
          then [[fmt|entropy: #{jpath} looks like a long hex token|]]
          else []
  let longBase64Finding =
        if looksLikeLongBase64 text
          then [[fmt|entropy: #{jpath} looks like a base64 blob|]]
          else []
  let shannonFinding =
        if Text.length text >= 20 && shannonEntropy text > 4.0
          then [[fmt|entropy: #{jpath} has high Shannon entropy|]]
          else []
  substringFindings ++ longHexFinding ++ longBase64Finding ++ shannonFinding


-- | At least 32 consecutive lowercase hex characters.
looksLikeLongHex :: Text -> Bool
looksLikeLongHex text = do
  let chars = Text.toLinkedList text
  Text.length text >= 32 && GhcList.all isHexLower chars
  where
    isHexLower c = GhcChar.isDigit c || (c >= 'a' && c <= 'f')


-- | Likely base64 blob (letters, digits, +/=) longer than 48 characters.
looksLikeLongBase64 :: Text -> Bool
looksLikeLongBase64 text = do
  let chars = Text.toLinkedList text
  Text.length text > 48 && GhcList.all isBase64Char chars
  where
    isBase64Char c =
      GhcChar.isAsciiUpper c
        || GhcChar.isAsciiLower c
        || GhcChar.isDigit c
        || c == '+'
        || c == '/'
        || c == '='


-- | Shannon entropy (bits per character) of a text.
shannonEntropy :: Text -> Double
shannonEntropy text = do
  let chars = Text.toLinkedList text
  let len = Prelude.fromIntegral (Text.length text) :: Double
  let counts = GhcList.foldl' (\m c -> GhcMap.insertWith (+) c (1 :: Int) m) GhcMap.empty chars
  let probs = GhcMap.elems counts |> GhcList.map (\c -> Prelude.fromIntegral c Prelude./ len)
  let entropyContribution p = if p > 0 then Prelude.negate (p Prelude.* Prelude.logBase 2.0 p) else 0.0
  GhcList.foldl' (\acc p -> acc Prelude.+ entropyContribution p) 0.0 probs
