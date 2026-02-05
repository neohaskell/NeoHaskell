-- | Wrapper type that redacts values in Show output.
--
-- Use 'Redacted' to prevent sensitive data from appearing in logs,
-- error messages, or anywhere that uses 'Show'.
--
-- == Quick Start
--
-- @
-- import Redacted (Redacted)
-- import Redacted qualified
--
-- -- Wrap sensitive values
-- let apiKey = Redacted.wrap "sk-xxx-secret"
-- let userMessage = Redacted.wrap userInput
--
-- -- Safe to log
-- log [fmt|Processing request with key: #{apiKey}|]
-- -- Output: Processing request with key: \<redacted\>
--
-- -- Explicit unwrapping required for use
-- sendToApi (Redacted.unwrap apiKey)
-- @
module Redacted (
  Redacted,
  wrap,
  labeled,
  unwrap,
  empty,
) where

import Appendable ((++))
import Control.Applicative (pure)
import Data.Aeson qualified as Aeson
import Data.String qualified as GhcString
import GHC.Show (Show (..))
import Text (Text)
import Text qualified


-- | A value that shows as @\<redacted\>@ in Show output.
--
-- Two constructors:
--
-- * 'Redacted' value: Shows as @\<redacted\>@ — most common case
-- * 'RedactedLabeled' label value: Shows as @\<redacted: label\>@ — for debugging complex structures
data Redacted value
  = Redacted value
  | RedactedLabeled Text value


-- | Wrap a value so it shows as @\<redacted\>@ in Show output.
--
-- @
-- let apiKey = Redacted.wrap "sk-xxx-secret"
-- show apiKey  -- "\<redacted\>"
-- @
wrap :: forall value. value -> Redacted value
wrap value = Redacted value


-- | Wrap a value with a label for debugging.
--
-- The label appears in Show output but the value is still hidden.
--
-- @
-- let apiKey = Redacted.labeled "api-key" "sk-xxx-secret"
-- show apiKey  -- "\<redacted: api-key\>"
-- @
labeled :: forall value. Text -> value -> Redacted value
labeled label value = RedactedLabeled label value


-- | Extract the wrapped value.
--
-- This requires explicit acknowledgment that you're accessing sensitive data.
--
-- @
-- let apiKey = Redacted.wrap "sk-xxx-secret"
-- sendToApi (Redacted.unwrap apiKey)  -- Explicit unwrap
-- @
unwrap :: forall value. Redacted value -> value
unwrap redacted =
  case redacted of
    Redacted value -> value
    RedactedLabeled _ value -> value


-- | Create an empty Redacted value for types with a Default-like empty.
--
-- Useful for initializing secret fields.
--
-- @
-- let emptyKey = Redacted.empty @Text
-- @
empty :: forall value. (GhcString.IsString value) => Redacted value
empty = Redacted (GhcString.fromString "")


-- | Show instance displays @\<redacted\>@ or @\<redacted: label\>@, never the actual value.
instance Show (Redacted value) where
  show redacted =
    case redacted of
      Redacted _ -> "<redacted>"
      RedactedLabeled label _ -> "<redacted: " ++ Text.toLinkedList label ++ ">"


-- | FromJSON is safe because parsed values become protected immediately.
instance (Aeson.FromJSON value) => Aeson.FromJSON (Redacted value) where
  parseJSON jsonValue = do
    value <- Aeson.parseJSON jsonValue
    pure (Redacted value)


-- NOTE: No ToJSON instance - prevents accidental serialization of secrets.
-- If you need to serialize, explicitly use: Aeson.toJSON (Redacted.unwrap value)

-- NOTE: No Eq instance - comparing secrets can leak information via timing attacks.
-- If needed, use: Redacted.unwrap a == Redacted.unwrap b

-- NOTE: No Generic instance - would allow deriving ToJSON/FromJSON, defeating safety.
