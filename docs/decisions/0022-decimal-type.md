# ADR-0022: Decimal Type for Financial Calculations

## Status

Proposed

## Context

NeoHaskell lacks a decimal or fixed-point number type for precise financial calculations. This gap forces downstream applications to use workarounds that are error-prone and verbose.

### Current State

1. **No decimal type in nhcore**: The core library provides `Int`, `Int64`, and `Float` (which is `Prelude.Double`), but no fixed-point decimal type suitable for monetary values.

2. **Workaround with Int64 cents**: Applications like the Neclau invoicing app store monetary values as `Int64` representing cents (e.g., `1250` for 12.50 EUR), requiring manual conversion at every boundary:
   ```haskell
   -- Current workaround - error-prone and verbose
   let priceInCents = 1250 :: Int64  -- 12.50 EUR
   let taxRate = 21 :: Int64         -- 21%
   let tax = (priceInCents * taxRate) `div` 100
   ```

3. **Float is unsuitable for money**: Using `Float` (Double) introduces precision errors:
   ```haskell
   -- Classic floating-point problem
   0.1 + 0.2 /= 0.3  -- True! (0.30000000000000004)
   ```

### Use Cases

- Product prices (12.50 EUR)
- Tax calculations (IVA 21%)
- Invoice line totals and grand totals
- Any financial or invoicing application built on NeoHaskell

### Design Goals

1. **Precision**: No floating-point errors in arithmetic
2. **Type safety**: Distinct type prevents mixing with raw integers or floats
3. **NeoHaskell style**: Qualified module design, pipe-friendly API
4. **Serialization**: JSON as string to avoid JavaScript precision loss
5. **Schema support**: Integration with NeoHaskell's `ToSchema` typeclass

### GitHub Issue

- [#330: Add Decimal/Money type for financial calculations](https://github.com/neohaskell/NeoHaskell/issues/330)

## Decision

### 1. Type Name: `Decimal`

We choose `Decimal` over alternatives:

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `Money` | Rejected | Too specific — implies currency, but the type is useful for any fixed-point arithmetic (percentages, quantities, etc.) |
| `FixedPoint` | Rejected | Too technical for NeoHaskell's newcomer-friendly philosophy |
| `Decimal` | **Chosen** | Familiar to developers from other languages (C#, Python, Java), self-documenting, no ecosystem conflicts |

### 2. Module Location: `core/decimal/Decimal.hs`

A new `decimal` source directory in `nhcore.cabal`:
- Follows the existing multi-source-dir pattern (core, json, schema, etc.)
- Re-exported from `Core.hs` for universal availability
- Keeps the module hierarchy flat and discoverable

### 3. Internal Representation

```haskell
newtype Decimal = Decimal { unDecimal :: Int64 }
  deriving (Eq, Ord, Generic)
```

The internal value represents the amount multiplied by 10,000 (4 decimal places):
- `12.50` is stored as `125000`
- `0.0001` is stored as `1`
- `99999999.9999` is stored as `999999999999`

**Why 4 decimal places?**
- 2 decimals is insufficient for intermediate tax calculations (21% of 12.50 = 2.625)
- 4 decimals provides enough precision for financial rounding while keeping Int64 range practical
- Int64 max (~9.2 * 10^18) divided by 10,000 gives a max value of ~922 trillion — more than sufficient

### 4. Arithmetic Design

**Custom `Num` instance** (not derived) because multiplication requires scaling:

```haskell
instance Prelude.Num Decimal where
  (Decimal a) + (Decimal b) = Decimal (a + b)
  (Decimal a) - (Decimal b) = Decimal (a - b)
  (Decimal a) * (Decimal b) = Decimal ((a * b) `Prelude.div` scale)
  abs (Decimal a) = Decimal (Prelude.abs a)
  signum (Decimal a) = Decimal (Prelude.signum a * scale)
  fromInteger n = Decimal (Prelude.fromInteger n * scale)
```

**No `Fractional` instance**: NeoHaskell redefines `/` as `Float -> Float -> Float` in `Basics.hs`. Making `Decimal` an instance of `Fractional` would create confusion. Instead, we provide:

```haskell
divide :: Decimal -> Decimal -> Decimal
divide (Decimal a) (Decimal b) = Decimal ((a * scale) `Prelude.div` b)
```

### 5. API Design

```haskell
-- Construction
decimal :: Float -> Decimal        -- From Float (NeoHaskell's Double)
fromCents :: Int64 -> Decimal      -- From cents (2 decimal places)
zero :: Decimal                    -- Zero value

-- Destruction
toCents :: Decimal -> Int64        -- To cents (truncates to 2 decimals)
toFloat :: Decimal -> Float        -- Lossy conversion for display

-- Arithmetic
divide :: Decimal -> Decimal -> Decimal

-- Formatting
formatDecimal :: Decimal -> Text   -- "12.50"
parseDecimal :: Text -> Maybe Decimal

-- Rounding
roundTo2 :: Decimal -> Decimal     -- Round to 2 decimal places
```

**Accessor naming rationale:**
- `decimal` mirrors `path` (Path module) — simple constructor from common type
- `fromCents`/`toCents` — domain-specific, self-documenting
- `divide` — explicit, avoids Fractional confusion
- Works with pipes: `price |> Decimal.toCents`, `"12.50" |> Decimal.parseDecimal`

### 6. Instance Decisions

| Instance | Decision | Rationale |
|----------|----------|-----------|
| `Eq` | **Yes** (derived) | Comparing decimal values is safe and expected |
| `Ord` | **Yes** (derived) | Ordering is needed for sorting, min/max |
| `Generic` | **Yes** (derived) | Needed for generic programming patterns |
| `Show` | **Custom** | Display as decimal string (e.g., `"12.50"`) not internal representation |
| `Num` | **Custom** | Multiplication needs scaling by 10,000 |
| `Fractional` | **No** | Conflicts with NeoHaskell's `/` operator redefinition |
| `ToJSON` | **Custom** | Serialize as JSON string to avoid JS precision loss |
| `FromJSON` | **Custom** | Parse from JSON string |
| `ToSchema` | **Custom** | Return `SText` since JSON representation is a string |
| `Default` | **Yes** | Default to zero |

### 7. JSON Serialization

Serialize as string, not number, to avoid JavaScript precision loss:

```json
{"price": "12.50"}
```

Not:
```json
{"price": 12.50}
```

This follows the same pattern used by many financial APIs (Stripe, PayPal).

### 8. Core.hs Integration

Add to `Core.hs` re-exports:
```haskell
import Decimal as Reexported (Decimal)
```

## Consequences

### Positive

1. **Eliminates floating-point errors**: All arithmetic uses integer operations internally, guaranteeing precision for financial calculations.

2. **Type-safe monetary values**: `Decimal` is distinct from `Int`, `Int64`, and `Float`, preventing accidental mixing of types.

3. **Consistent serialization**: JSON string format avoids JavaScript precision issues and is compatible with financial API conventions.

4. **NeoHaskell style compliant**: Qualified module design (`Decimal.fromCents`, `Decimal.formatDecimal`), pipe-friendly, no Haskell-isms.

5. **Schema integration**: Works with NeoHaskell's `ToSchema` for automatic OpenAPI documentation.

6. **Unblocks downstream apps**: The Neclau invoicing application can use `Decimal` directly instead of manual Int64 cent conversions.

### Negative

1. **Slightly more complex than Double**: Developers must use `Decimal.decimal 12.50` instead of just `12.50`, though `fromInteger` allows integer literals.

2. **Fixed precision**: 4 decimal places may not suit all use cases (e.g., cryptocurrency with 8+ decimals). This is acceptable for the primary use case (financial/invoicing).

3. **No Fractional instance**: Division requires `Decimal.divide` instead of `/`. This is intentional but may surprise developers coming from other languages.

### Risks

1. **Overflow on very large values**: Mitigated by Int64 range (~922 trillion with 4 decimal places). Document the limits.

2. **Division rounding**: Integer division truncates towards zero. This must be clearly documented, and `roundTo2` provided for explicit rounding.

3. **Float conversion precision**: `decimal 12.50` converts through Double, which could introduce tiny errors. Mitigated by using `Prelude.round` during conversion.

## References

- [GitHub Issue #330](https://github.com/neohaskell/NeoHaskell/issues/330) — Original feature request
- [Basics.hs](../../core/core/Basics.hs) — NeoHaskell's arithmetic operator definitions
- [Schema.hs](../../core/schema/Schema.hs) — ToSchema typeclass
- [Json.hs](../../core/json/Json.hs) — JSON serialization wrapper
- [Redacted.hs](../../core/core/Redacted.hs) — Reference newtype pattern
- [ADR-0013: Automatic Schema Generation](0013-automatic-schema-generation.md) — Schema system context
