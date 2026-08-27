-- | Reproduction + criteria for change 007 (issue #729): `DateTime` has no
-- `ToSchema` instance, so any read-model record carrying a timestamp field
-- fails to compile under generic `Schema` derivation.
--
-- This module is committed RED. A missing instance is a *type* error, so
-- "red" here is a compile failure of the `nhcore-test-core` suite
-- (@No instance for (ToSchema DateTime)@), not a failing runtime assertion.
-- The fix — `instance ToSchema DateTime where toSchema = SText` in
-- `core/schema/Schema.hs` — turns it green without changing an assertion.
--
-- Spec: docs/changes/007-729-datetime-toschema.md
module Schema.DateTimeSpec (spec) where

import Array qualified
import Basics
import Control.Lens qualified as Lens
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens qualified as OpenApiLens
import DateTime (DateTime)
import DateTime qualified
import Json qualified
import Maybe (Maybe (..))
import Schema (Schema (..), ToSchema (..))
import Schema.JsonSchema qualified
import Schema.OpenApi qualified
import Test (Spec)
import Test qualified
import Text (Text)
import Uuid (Uuid)


-- | The record from issue #729: a read-model row carrying a timestamp, plus
-- the `Maybe DateTime` variant the issue reports as failing the same way.
data Row = Row
  { rowId :: Uuid
  , occurredAt :: DateTime
  , deletedAt :: Maybe DateTime
  }
  deriving (Generic)


instance ToSchema Row


-- | 1970-01-01T00:00:00Z — a whole second, so the ISO-8601 rendering carries
-- no subsecond part.
epoch :: DateTime
epoch = DateTime.fromEpochSeconds 0


spec :: Spec Unit
spec = do
  Test.describe "ToSchema DateTime" do
    -- C2
    Test.it "generates SText for DateTime" \_ -> do
      toSchema @DateTime
        |> Test.shouldBe SText

    -- C3
    Test.it "generates SOptional SText for Maybe DateTime" \_ -> do
      toSchema @(Maybe DateTime)
        |> Test.shouldBe (SOptional SText)

    -- C4 — the anti-tautology guard: SText is only honest if the encoder
    -- really emits a JSON string. Asserts the wire form independently of
    -- the schema, so C2 cannot pass by merely restating itself.
    Test.it "encodes DateTime as an ISO-8601 JSON string, matching SText" \_ -> do
      Json.encodeText epoch
        |> Test.shouldBe "\"1970-01-01T00:00:00Z\""

    -- C1 — the issue's reproduction.
    Test.it "derives a schema for a record with DateTime fields" \_ -> do
      let schema = toSchema @Row
      case schema of
        SObject fields -> do
          Array.length fields |> Test.shouldBe 3
          let occurredAtField = fields |> Array.find (\f -> f.fieldName == "occurredAt")
          let deletedAtField = fields |> Array.find (\f -> f.fieldName == "deletedAt")
          case (occurredAtField, deletedAtField) of
            (Just occurred, Just deleted) -> do
              occurred.fieldSchema |> Test.shouldBe SText
              occurred.fieldRequired |> Test.shouldBe True
              deleted.fieldSchema |> Test.shouldBe (SOptional SText)
              deleted.fieldRequired |> Test.shouldBe False
            _ -> do
              "Row schema is missing its DateTime fields"
                |> Test.shouldBe ("Row schema has both DateTime fields" :: Text)
        _ -> do
          "Row schema is not an SObject"
            |> Test.shouldBe ("Row schema is an SObject" :: Text)

    -- C5
    Test.it "lowers DateTime to JSON Schema type string" \_ -> do
      Schema.JsonSchema.toJsonSchema (toSchema @DateTime)
        |> Test.shouldBe (Json.object [("type", Json.toJSON ("string" :: Text))])

    -- C6 — pins the deliberate scope boundary: no `format: date-time` today.
    Test.it "lowers DateTime to OpenApiString with no format" \_ -> do
      let openApiSchema = Schema.OpenApi.toOpenApiSchema (toSchema @DateTime)
      openApiSchema
        |> Lens.view OpenApiLens.type_
        |> Test.shouldBe (Just OpenApi.OpenApiString)
      openApiSchema
        |> Lens.view OpenApiLens.format
        |> Test.shouldBe (Nothing :: Maybe Text)
