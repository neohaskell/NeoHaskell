module Syntax.FunctionSpec (spec) where

import Core
import Array qualified
import Result qualified
import Parser qualified
import Syntax.Function (BlockStatement (..), ConstantDef (..), FunctionBody (..), FunctionDef (..), TopLevelDecl (..), TypeConstraint (..))
import Syntax.Function qualified
import Layout qualified
import Test


spec :: Spec Unit
spec = do
  describe "functionDef" do
    it "parses a pure expression function" \_ -> do
      let source = "fun add(x: Int, y: Int) : Int = x + y"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { name = functionName, parameters = params, returnType = declaredReturnType, body = Expression { exprText = expressionText } }) -> do
          functionName |> shouldBe "add"
          params |> Array.length |> shouldBe 2
          declaredReturnType |> shouldBe "Int"
          expressionText |> shouldBe "x + y"
        Ok other ->
          fail [fmt|Expected expression function, got: #{toText other}|]

    it "parses a block function" \_ -> do
      let source = "fun distance(x1: Float, x2: Float) : Float {\n  let dx = x2 - x1\n  sqrt(dx)\n}"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { body = Block { statements = stmts } }) -> do
          stmts |> Array.length |> shouldBe 2
          case stmts |> Array.get 0 of
            Nothing ->
              fail [fmt|Expected first statement, got none|]
            Just (PureBinding { bindingName = nameText, bindingValue = valueText }) -> do
              nameText |> shouldBe "dx"
              valueText |> shouldBe "x2 - x1"
            Just other ->
              fail [fmt|Expected PureBinding, got: #{toText other}|]
        Ok other ->
          fail [fmt|Expected block function, got: #{toText other}|]

    it "parses an effectful block with let!" \_ -> do
      let source = "fun load(path: Text) : Task<Text> {\n  let! content = File.read(path)\n  Task.yield(content)\n}"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { body = Block { statements = stmts } }) ->
          case stmts |> Array.get 0 of
            Nothing ->
              fail [fmt|Expected first statement, got none|]
            Just (EffectfulBinding { bindingName = nameText, bindingValue = valueText }) -> do
              nameText |> shouldBe "content"
              valueText |> shouldBe "File.read(path)"
            Just other ->
              fail [fmt|Expected EffectfulBinding, got: #{toText other}|]
        Ok other ->
          fail [fmt|Expected block function, got: #{toText other}|]

    it "parses a generic function" \_ -> do
      let source = "fun identity<t>(x: t) : t = x"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { typeParams = params, returnType = declaredReturnType }) -> do
          params |> shouldBe ["t"]
          declaredReturnType |> shouldBe "t"

    it "parses a constrained function" \_ -> do
      let source = "fun show<t>(x: t) : Text where t: Show { toString(x) }"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { constraints = parsedConstraints }) ->
          case parsedConstraints |> Array.get 0 of
            Nothing ->
              fail [fmt|Expected a constraint, got none|]
            Just (TraitConstraint { variable = variableName, trait = traitNameText }) -> do
              variableName |> shouldBe "t"
              traitNameText |> shouldBe "Show"
            Just other ->
              fail [fmt|Expected TraitConstraint, got: #{toText other}|]

    it "parses a zero-argument function" \_ -> do
      let source = "fun getTime() : Task<Time> { Task.yield(now) }"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { parameters = params }) ->
          params |> shouldBe Array.empty

    it "parses a field constraint" \_ -> do
      let source = "fun greet<a>(s: a) : Text where a has name: Text { s.name }"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { constraints = parsedConstraints }) ->
          case parsedConstraints |> Array.get 0 of
            Nothing ->
              fail [fmt|Expected a constraint, got none|]
            Just (FieldConstraint { variable = variableName, fieldName = requiredFieldName, fieldType = requiredFieldType }) -> do
              variableName |> shouldBe "a"
              requiredFieldName |> shouldBe "name"
              requiredFieldType |> shouldBe "Text"
            Just other ->
              fail [fmt|Expected FieldConstraint, got: #{toText other}|]

    it "parses a multi-parameter constraint" \_ -> do
      let source = "fun convert<a, b>(x: a) : b where Convertible<a, b> { Convertible.convert(x) }"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { constraints = parsedConstraints }) ->
          case parsedConstraints |> Array.get 0 of
            Nothing ->
              fail [fmt|Expected a constraint, got none|]
            Just (MultiParamConstraint { traitName = className, typeArgs = arguments }) -> do
              className |> shouldBe "Convertible"
              arguments |> shouldBe ["a", "b"]
            Just other ->
              fail [fmt|Expected MultiParamConstraint, got: #{toText other}|]

    it "parses field constraints before trait constraints (ordering regression guard)" \_ -> do
      let source = "fun greet<a>(s: a) : Text where a has name: Text { s.name }"
      let result = source |> Parser.run Syntax.Function.functionDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDef { constraints = parsedConstraints }) ->
          case parsedConstraints |> Array.get 0 of
            Just FieldConstraint {} ->
              pass
            Just other ->
              fail [fmt|Expected FieldConstraint first, got: #{toText other}|]
            Nothing ->
              fail [fmt|Expected one FieldConstraint, got none|]

  describe "constantDef" do
    it "parses a simple constant" \_ -> do
      let source = "let pi : Float = 3.14"
      let result = source |> Parser.run Syntax.Function.constantDef
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (ConstantDef { name = constantName, constType = declaredType, value = constantValue }) -> do
          constantName |> shouldBe "pi"
          declaredType |> shouldBe "Float"
          constantValue |> shouldBe "3.14"

  describe "functionDef (error cases)" do
    it "fails when return type is missing" \_ -> do
      let source = "fun add(x: Int, y: Int) = x + y"
      let result = source |> Parser.run Syntax.Function.functionDef
      result |> Result.isErr |> shouldBe True

    it "fails when expression body is empty" \_ -> do
      let source = "fun add(x: Int, y: Int) : Int ="
      let result = source |> Parser.run Syntax.Function.functionDef
      result |> Result.isErr |> shouldBe True

    it "fails when block body is empty" \_ -> do
      let source = "fun add(x: Int, y: Int) : Int {}"
      let result = source |> Parser.run Syntax.Function.functionDef
      result |> Result.isErr |> shouldBe True

    it "fails when constant value is empty" \_ -> do
      let source = "let pi : Float ="
      let result = source |> Parser.run Syntax.Function.constantDef
      result |> Result.isErr |> shouldBe True

  describe "topLevelDecl" do
    it "dispatches to function declarations" \_ -> do
      let source = "fun add(x: Int, y: Int) : Int = x + y"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (FunctionDecl FunctionDef {}) ->
          pass
        Ok other ->
          fail [fmt|Expected FunctionDecl, got: #{toText other}|]

    it "dispatches to constant declarations" \_ -> do
      let source = "let answer : Int = 42"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (ConstantDecl ConstantDef {}) ->
          pass
        Ok other ->
          fail [fmt|Expected ConstantDecl, got: #{toText other}|]

  describe "toHaskell" do
    it "transpiles expression functions" \_ -> do
      let source = "fun add(x: Int, y: Int) : Int = x + y"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok declaration ->
          declaration
            |> Syntax.Function.toHaskell
            |> Layout.render
            |> shouldBe "add :: Int -> Int -> Int\nadd x y = x + y"

    it "transpiles block functions" \_ -> do
      let source = "fun distance(x1: Float, x2: Float) : Float {\n  let dx = x2 - x1\n  sqrt(dx)\n}"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok declaration ->
          declaration
            |> Syntax.Function.toHaskell
            |> Layout.render
            |> shouldBe "distance :: Float -> Float -> Float\ndistance x1 x2 = do\n  let dx = x2 - x1\n  sqrt(dx)"

    it "transpiles effectful blocks and strips return markers" \_ -> do
      let source = "fun getTime() : Task<Time> {\n  let! now = Clock.now()\n  return Task.yield(now)\n}"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok declaration ->
          declaration
            |> Syntax.Function.toHaskell
            |> Layout.render
            |> shouldBe "getTime :: Task Time\ngetTime = do\n  now <- Clock.now()\n  Task.yield(now)"

    it "transpiles constrained functions" \_ -> do
      let source = "fun show<t>(x: t) : Text where t: Show { toString(x) }"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok declaration ->
          declaration
            |> Syntax.Function.toHaskell
            |> Layout.render
            |> shouldBe "show :: (Show t) => t -> Text\nshow x = do\n  toString(x)"

    it "transpiles constants" \_ -> do
      let source = "let pi : Float = 3.14"
      let result = source |> Parser.run Syntax.Function.topLevelDecl
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok declaration ->
          declaration
            |> Syntax.Function.toHaskell
            |> Layout.render
            |> shouldBe "pi :: Float\npi = 3.14"
