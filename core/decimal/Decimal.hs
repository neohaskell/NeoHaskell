-- | Fixed-point decimal type for precise financial calculations.
--
-- @Decimal@ stores values with 4 internal decimal places using @Int64@,
-- eliminating floating-point precision errors common with @Float@/@Double@.
--
-- == Quick Start
--
-- @
-- import Decimal (Decimal)
-- import Decimal qualified
--
-- -- Construction
-- let price = Decimal.decimal 12.50
-- let tax = Decimal.decimal 2.625
-- let total = price + tax
--
-- -- Formatting
-- Decimal.formatDecimal total  -- "15.1250"
--
-- -- JSON serialization (as string)
-- Json.toJSON price  -- String "12.5000"
-- @
--
-- == Internal Representation
--
-- Values are stored multiplied by 10,000:
--
-- @
-- 12.50   -> 125000
-- 0.0001  -> 1
-- -5.25   -> -52500
-- @
module Decimal (
  Decimal (..),
  decimal,
  fromCents,
  toCents,
  toFloat,
  divide,
  formatDecimal,
  parseDecimal,
  roundTo2,
  zero,
) where

import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.Char qualified as GhcChar
import Data.Text qualified as GhcText
import Default (Default (..))
import Maybe (Maybe (..))
import Prelude qualified
import Schema (Schema (..), ToSchema (..))
import Text (Text)
import Text qualified


-- | Fixed-point decimal with 4 internal decimal places.
--
-- Internal representation: value * 10000
--
-- @
-- Decimal 125000  -- represents 12.5000
-- Decimal 1       -- represents 0.0001
-- Decimal (-52500) -- represents -5.2500
-- @
newtype Decimal = Decimal {unDecimal :: Int64}
  deriving (Eq, Ord, Generic)


-- | Internal scale factor (10^4 = 10000)
scale :: Int64
scale = 10000
{-# INLINE scale #-}


-- * Construction


-- | Create a @Decimal@ from a @Float@ value.
--
-- @
-- Decimal.decimal 12.50   -- Decimal 125000
-- Decimal.decimal 0.01    -- Decimal 100
-- Decimal.decimal (-5.25) -- Decimal (-52500)
-- @
decimal :: Float -> Decimal
decimal d = Decimal (Prelude.round (d * Prelude.fromIntegral scale))
{-# INLINE decimal #-}


-- | Create a @Decimal@ from a cents value (2 decimal places).
--
-- @
-- Decimal.fromCents 1250  -- same as Decimal.decimal 12.50
-- Decimal.fromCents 100   -- same as Decimal.decimal 1.00
-- @
fromCents :: Int64 -> Decimal
fromCents cents = Decimal (cents * 100)
{-# INLINE fromCents #-}


-- | The zero value.
--
-- @
-- Decimal.zero == Decimal.decimal 0.00
-- @
zero :: Decimal
zero = Decimal 0
{-# INLINE zero #-}


-- * Destruction


-- | Convert a @Decimal@ to cents (truncates to 2 decimal places).
--
-- @
-- Decimal.decimal 12.50 |> Decimal.toCents  -- 1250
-- Decimal.decimal 12.999 |> Decimal.toCents -- 1299
-- @
toCents :: Decimal -> Int64
toCents (Decimal n) = Prelude.div n 100
{-# INLINE toCents #-}


-- | Convert a @Decimal@ to a @Float@. This is a lossy conversion
-- and should only be used for display purposes.
--
-- @
-- Decimal.decimal 12.50 |> Decimal.toFloat  -- 12.5
-- @
toFloat :: Decimal -> Float
toFloat (Decimal n) = Prelude.fromIntegral n / Prelude.fromIntegral scale
{-# INLINE toFloat #-}


-- * Arithmetic


-- | Divide two @Decimal@ values.
--
-- NeoHaskell redefines @/@ as @Float -> Float -> Float@, so we provide
-- this explicit division function instead of a @Fractional@ instance.
--
-- @
-- Decimal.divide (Decimal.decimal 100.00) (Decimal.decimal 4.00) -- Decimal.decimal 25.00
-- @
divide :: Decimal -> Decimal -> Decimal
divide (Decimal a) (Decimal b) = Decimal (Prelude.div (a * scale) b)
{-# INLINE divide #-}


-- * Instances


instance Prelude.Num Decimal where
  (Decimal a) + (Decimal b) = Decimal (a + b)
  {-# INLINE (+) #-}

  (Decimal a) - (Decimal b) = Decimal (a - b)
  {-# INLINE (-) #-}

  (Decimal a) * (Decimal b) = Decimal (Prelude.div (a * b) scale)
  {-# INLINE (*) #-}

  abs (Decimal a) = Decimal (Prelude.abs a)
  {-# INLINE abs #-}

  signum (Decimal a) = Decimal (Prelude.signum a * scale)
  {-# INLINE signum #-}

  fromInteger n = Decimal (Prelude.fromInteger n * scale)
  {-# INLINE fromInteger #-}


instance Show Decimal where
  show d = Text.toLinkedList (formatDecimal d)


instance Default Decimal where
  def = zero


instance Aeson.ToJSON Decimal where
  toJSON d = Aeson.String (formatDecimal d)


instance Aeson.FromJSON Decimal where
  parseJSON = Aeson.withText "Decimal" parseJsonDecimal


parseJsonDecimal :: Text -> AesonTypes.Parser Decimal
parseJsonDecimal t =
  case parseDecimal t of
    Just d -> Prelude.pure d
    Nothing -> Prelude.fail (Text.toLinkedList [fmt|Invalid decimal format: #{t}|])


instance ToSchema Decimal where
  toSchema = SText


-- * Formatting


-- | Format a @Decimal@ as @Text@ with 4 decimal places.
--
-- @
-- Decimal.decimal 12.50 |> Decimal.formatDecimal   -- "12.5000"
-- Decimal.decimal (-5.25) |> Decimal.formatDecimal  -- "-5.2500"
-- Decimal.decimal 0.00 |> Decimal.formatDecimal     -- "0.0000"
-- @
formatDecimal :: Decimal -> Text
formatDecimal (Decimal n) = do
  let isNeg = n < 0
  let absN = Prelude.abs n
  let wholePart = Prelude.div absN scale
  let fracPart = Prelude.mod absN scale
  let wholeText = Text.fromLinkedList (Prelude.show wholePart)
  let fracText = padLeft4 (Text.fromLinkedList (Prelude.show fracPart))
  case isNeg of
    True -> [fmt|-#{wholeText}.#{fracText}|]
    False -> [fmt|#{wholeText}.#{fracText}|]
{-# INLINE formatDecimal #-}


-- | Pad a text to 4 characters with leading zeros.
padLeft4 :: Text -> Text
padLeft4 t = do
  let len = Text.length t
  case len >= 4 of
    True -> t
    False -> do
      let padding = GhcText.replicate (4 - len) "0"
      padding `GhcText.append` t


-- * Parsing


-- | Parse a @Text@ value into a @Decimal@.
--
-- @
-- Decimal.parseDecimal "12.50"   -- Just (Decimal.decimal 12.50)
-- Decimal.parseDecimal "-5.25"   -- Just (Decimal.decimal (-5.25))
-- Decimal.parseDecimal "abc"     -- Nothing
-- Decimal.parseDecimal ""        -- Nothing
-- @
parseDecimal :: Text -> Maybe Decimal
parseDecimal t = do
  let stripped = GhcText.strip t
  case GhcText.null stripped of
    True -> Nothing
    False -> do
      let str = Text.toLinkedList stripped
      case readDouble str of
        Just d -> Just (decimal d)
        Nothing -> Nothing


-- | Try to read a Double from a String
readDouble :: [Prelude.Char] -> Maybe Float
readDouble str =
  case str of
    [] -> Nothing
    _ ->
      case isValidDecimalString str of
        False -> Nothing
        True ->
          case Prelude.reads str of
            [(d, "")] -> Just d
            _ -> Nothing


-- | Check if a string looks like a valid decimal number
isValidDecimalString :: [Prelude.Char] -> Bool
isValidDecimalString str =
  case str of
    [] -> False
    ('-' : rest) -> isValidUnsignedDecimal rest
    ('+' : rest) -> isValidUnsignedDecimal rest
    _ -> isValidUnsignedDecimal str


isValidUnsignedDecimal :: [Prelude.Char] -> Bool
isValidUnsignedDecimal str =
  case str of
    [] -> False
    _ -> Prelude.all (\c -> GhcChar.isDigit c || c == '.') str
      && Prelude.length (Prelude.filter (== '.') str) <= 1
      && Prelude.any GhcChar.isDigit str


-- * Rounding


-- | Round a @Decimal@ to 2 decimal places.
--
-- @
-- Decimal.decimal 12.5678 |> Decimal.roundTo2  -- Decimal with internal value 125700
-- @
roundTo2 :: Decimal -> Decimal
roundTo2 (Decimal n) = do
  let rounded = Prelude.div (n + 50 * Prelude.signum n) 100 * 100
  Decimal rounded
{-# INLINE roundTo2 #-}
