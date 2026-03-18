module Main (main) where

import Prelude (IO)

import Command.FromSchemaSpec qualified
import ConcurrentMapSpec qualified
import DecimalSpec qualified
import EventVariantOfSpec qualified
import Config.ApplicationSpec qualified
import Config.ConfigDependentSpec qualified
import Config.OAuth2ConfigDependentSpec qualified
import Config.BuilderSpec qualified
import Config.CoreSpec qualified
import Config.ErrorSafetySpec qualified
import Config.InputValidationSpec qualified
import Config.RuntimeSafetySpec qualified
import Config.SecretSpec qualified
import Config.THSpec qualified
import Http.ClientRawSpec qualified
import Http.ClientSpec qualified
import IntSpec qualified
import Layout.RenderSpec qualified
import LayoutSpec qualified
import NeoQL.ExecuteSpec qualified
import NeoQL.ParserSpec qualified
import RedactedSpec qualified
import Schema.OpenApiSpec qualified
import SchemaSpec qualified
import Schema.JsonSchemaSpec qualified
import Service.Transport.Web.SwaggerUISpec qualified
import SetSpec qualified
import StreamSpec qualified
import Test.AppSpec.AppSpecSpec qualified
import Test.AppSpec.VerifySpec qualified
import ParserSpec qualified
import Parser.ErrorSpec qualified
import Parser.WhitespaceSpec qualified
import Parser.FileSpec qualified
import Syntax.CommentSpec qualified
import Syntax.FunctionSpec qualified
import TaskSpec qualified
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "Command.FromSchema" Command.FromSchemaSpec.spec
  Hspec.describe "ConcurrentMap" ConcurrentMapSpec.spec
  Hspec.describe "Decimal" DecimalSpec.spec
  Hspec.describe "EventVariantOf" EventVariantOfSpec.spec
  Hspec.describe "Config.Application" Config.ApplicationSpec.spec
  Hspec.describe "Config.ConfigDependent" Config.ConfigDependentSpec.spec
  Hspec.describe "Config.OAuth2ConfigDependent" Config.OAuth2ConfigDependentSpec.spec
  Hspec.describe "Config.Builder" Config.BuilderSpec.spec
  Hspec.describe "Config.Core" Config.CoreSpec.spec
  Hspec.describe "Config.ErrorSafety" Config.ErrorSafetySpec.spec
  Hspec.describe "Config.InputValidation" Config.InputValidationSpec.spec
  Hspec.describe "Config.RuntimeSafety" Config.RuntimeSafetySpec.spec
  Hspec.describe "Config.Secret" Config.SecretSpec.spec
  Hspec.describe "Config.TH" Config.THSpec.spec
  Hspec.describe "Http.Client" Http.ClientSpec.spec
  Hspec.describe "Http.ClientRaw" Http.ClientRawSpec.spec
  Hspec.describe "Int" IntSpec.spec
  Hspec.describe "Layout" LayoutSpec.spec
  Hspec.describe "Layout.Render" Layout.RenderSpec.spec
  Hspec.describe "NeoQL.Parser" NeoQL.ParserSpec.spec
  Hspec.describe "NeoQL.Execute" NeoQL.ExecuteSpec.spec
  Hspec.describe "Redacted" RedactedSpec.spec
  Hspec.describe "Schema" SchemaSpec.spec
  Hspec.describe "Schema.OpenApi" Schema.OpenApiSpec.spec
  Hspec.describe "Schema.JsonSchema" Schema.JsonSchemaSpec.spec
  Hspec.describe "Service.Transport.Web.SwaggerUI" Service.Transport.Web.SwaggerUISpec.spec
  Hspec.describe "Set" SetSpec.spec
  Hspec.describe "Stream" StreamSpec.spec
  Hspec.describe "Test.AppSpec.AppSpec" Test.AppSpec.AppSpecSpec.spec
  Hspec.describe "Test.AppSpec.Verify" Test.AppSpec.VerifySpec.spec
  Hspec.describe "Parser" ParserSpec.spec
  Hspec.describe "Parser.Error" Parser.ErrorSpec.spec
  Hspec.describe "Parser.Whitespace" Parser.WhitespaceSpec.spec
  Hspec.describe "Parser.File" Parser.FileSpec.spec
  Hspec.describe "Syntax.Comment" Syntax.CommentSpec.spec
  Hspec.describe "Syntax.Function" Syntax.FunctionSpec.spec
  Hspec.describe "Task" TaskSpec.spec
