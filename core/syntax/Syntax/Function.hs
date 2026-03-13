-- | NeoHaskell function syntax parsing and transpilation.
--
-- Parsers for top-level @fun@ and @let@ declarations that __capture__ their
-- structure as an AST for the transpiler, formatter, and IDE tooling.
--
-- == Quick Start
--
-- @
-- import Syntax.Function (TopLevelDecl (..))
-- import Syntax.Function qualified
-- import Parser qualified
--
-- result :: Result Parser.ParseError TopLevelDecl
-- result = "fun add(x: Int, y: Int) : Int = x + y" |> Parser.run Syntax.Function.topLevelDecl
-- @
--
-- == Transpilation
--
-- @
-- declaration |> Syntax.Function.toHaskell |> Layout.render
-- @
--
-- Produces valid Haskell declaration syntax:
--
-- * @fun add(x: Int, y: Int) : Int = x + y@ →
--   @add :: Int -> Int -> Int\nadd x y = x + y@
-- * @fun getTime() : Task<Time> { let! now = Clock.now()\nTask.yield(now) }@ →
--   @getTime :: Task Time\ngetTime = do\n  now <- Clock.now()\n  Task.yield(now)@
-- * @let pi : Float = 3.14@ →
--   @pi :: Float\npi = 3.14@
module Syntax.Function
  ( -- * Types
    TopLevelDecl (..)
  , FunctionDef (..)
  , ConstantDef (..)
  , Parameter (..)
  , TypeConstraint (..)
  , FunctionBody (..)
  , BlockStatement (..)
    -- * Parsers
  , topLevelDecl
  , functionDef
  , constantDef
    -- * Transpilation
  , toHaskell
  ) where

import Basics
import Array (Array)
import Array qualified
import Parser (Parser, ParsePosition (..))
import Parser qualified
import Text (Text)
import Text qualified
import Prelude qualified
import Layout (Blueprint)
import Layout qualified
import Maybe (Maybe (..))


-- | A top-level declaration parsed from NeoHaskell source.
data TopLevelDecl
  = FunctionDecl FunctionDef
  | ConstantDecl ConstantDef
  deriving (Eq, Prelude.Show, Generic)


-- | A parsed @fun@ function definition.
data FunctionDef = FunctionDef
  { position    :: ParsePosition
  , name        :: Text
  , typeParams  :: Array Text
  , parameters  :: Array Parameter
  , returnType  :: Text
  , constraints :: Array TypeConstraint
  , body        :: FunctionBody
  }
  deriving (Eq, Prelude.Show, Generic)


-- | A parsed top-level constant definition.
data ConstantDef = ConstantDef
  { position  :: ParsePosition
  , name      :: Text
  , constType :: Text
  , value     :: Text
  }
  deriving (Eq, Prelude.Show, Generic)


-- | A function parameter and its type annotation.
data Parameter = Parameter
  { paramName :: Text
  , paramType :: Text
  }
  deriving (Eq, Prelude.Show, Generic)


-- | A single @where@ constraint.
data TypeConstraint
  = TraitConstraint
      { variable :: Text
      , trait    :: Text
      }
  | FieldConstraint
      { variable  :: Text
      , fieldName :: Text
      , fieldType :: Text
      }
  | MultiParamConstraint
      { traitName :: Text
      , typeArgs  :: Array Text
      }
  deriving (Eq, Prelude.Show, Generic)


-- | A function body: expression form or brace block form.
data FunctionBody
  = Expression
      { exprText :: Text
      }
  | Block
      { statements :: Array BlockStatement
      }
  deriving (Eq, Prelude.Show, Generic)


-- | A statement inside a brace block.
data BlockStatement
  = PureBinding
      { bindingName  :: Text
      , bindingValue :: Text
      }
  | EffectfulBinding
      { bindingName  :: Text
      , bindingValue :: Text
      }
  | BareExpression
      { exprText :: Text
      }
  | ReturnStatement
      { exprText :: Text
      }
  deriving (Eq, Prelude.Show, Generic)


data ExpressionScanState = ExpressionScanState
  { parenDepth      :: {-# UNPACK #-} Int
  , bracketDepth    :: {-# UNPACK #-} Int
  , braceDepth      :: {-# UNPACK #-} Int
  , inStringLiteral :: Bool
  , isEscaped       :: Bool
  }
  deriving (Eq, Prelude.Show, Generic)


data TypeScanState = TypeScanState
  { typeParenDepth   :: {-# UNPACK #-} Int
  , typeBracketDepth :: {-# UNPACK #-} Int
  , typeAngleDepth   :: {-# UNPACK #-} Int
  }
  deriving (Eq, Prelude.Show, Generic)


-- | Parse a top-level NeoHaskell declaration (@fun@ or top-level @let@).
topLevelDecl :: Parser TopLevelDecl
topLevelDecl =
  Parser.choice
    [ functionDef |> Parser.map FunctionDecl
    , constantDef |> Parser.map ConstantDecl
    ]
{-# INLINE topLevelDecl #-}


-- | Parse a @fun@ function definition.
functionDef :: Parser FunctionDef
functionDef = do
  pos <- Parser.position
  _ <- parseKeywordWithRequiredSpace "fun"
  functionName <- lowerIdentifier
  maybeTypeParams <- Parser.optional typeParameterList
  parsedParameters <-
    Parser.choice
      [ parameterList
      , Parser.problem [fmt|function '#{functionName}' has no parameter list\nhint: use '#{functionName}()' for a zero-argument function|]
      ]
  _ <- Parser.spaces
  _ <- Parser.char ':'
  _ <- Parser.spaces
  parsedReturnType <- captureReturnType
  _ <- Parser.spaces
  parsedConstraints <- whereClause
  _ <- Parser.spaces
  parsedBody <- functionBody
  let parsedTypeParams =
        case maybeTypeParams of
          Nothing -> Array.empty
          Just params -> params
  Parser.yield
    FunctionDef
      { position = pos
      , name = functionName
      , typeParams = parsedTypeParams
      , parameters = parsedParameters
      , returnType = parsedReturnType
      , constraints = parsedConstraints
      , body = parsedBody
      }
{-# INLINE functionDef #-}


-- | Parse a top-level @let@ constant definition.
constantDef :: Parser ConstantDef
constantDef = do
  pos <- Parser.position
  _ <- parseKeywordWithRequiredSpace "let"
  constantName <- lowerIdentifier
  _ <- Parser.spaces
  _ <- Parser.char ':'
  _ <- Parser.spaces
  parsedType <- captureConstantType
  _ <- Parser.char '='
  _ <- Parser.spaces
  parsedValue <- captureExpressionUntil Parser.end
  Parser.yield
    ConstantDef
      { position = pos
      , name = constantName
      , constType = parsedType
      , value = parsedValue
      }
{-# INLINE constantDef #-}


parseKeywordWithRequiredSpace :: Text -> Parser Unit
parseKeywordWithRequiredSpace keyword = do
  _ <- Parser.text keyword
  _ <- Parser.notFollowedBy Parser.alphaNum
  _ <- Parser.whitespace
  Parser.yield unit


identifierTailChar :: Parser Prelude.Char
identifierTailChar =
  Parser.choice
    [ Parser.alphaNum
    , Parser.char '_'
    ]


lowerIdentifier :: Parser Text
lowerIdentifier = do
  firstChar <- Parser.lower
  restChars <- identifierTailChar |> Parser.zeroOrMore
  let chars = Array.wrap firstChar |> Array.append restChars
  Parser.yield (chars |> Text.fromArray)


upperIdentifier :: Parser Text
upperIdentifier = do
  firstChar <- Parser.upper
  restChars <- identifierTailChar |> Parser.zeroOrMore
  let chars = Array.wrap firstChar |> Array.append restChars
  Parser.yield (chars |> Text.fromArray)


bindingIdentifier :: Parser Text
bindingIdentifier =
  Parser.choice
    [ lowerIdentifier
    , do
        _ <- Parser.char '_'
        restChars <- identifierTailChar |> Parser.zeroOrMore
        let chars = Array.wrap '_' |> Array.append restChars
        Parser.yield (chars |> Text.fromArray)
    ]


typeParameterList :: Parser (Array Text)
typeParameterList = do
  _ <- Parser.char '<'
  _ <- Parser.spaces
  params <- lowerIdentifier |> Parser.separatedBy commaSeparator
  _ <- Parser.spaces
  _ <- Parser.char '>'
  if params |> Array.isEmpty
    then Parser.problem "type parameter list cannot be empty"
    else Parser.yield params


parameterList :: Parser (Array Parameter)
parameterList = do
  _ <- Parser.char '('
  _ <- Parser.spaces
  params <- parameter |> Parser.separatedBy commaSeparator
  _ <- Parser.spaces
  _ <- Parser.char ')'
  Parser.yield params


parameter :: Parser Parameter
parameter = do
  parsedName <- bindingIdentifier
  _ <- Parser.spaces
  _ <- Parser.char ':'
  _ <- Parser.spaces
  parsedType <- captureParameterType
  Parser.yield
    Parameter
      { paramName = parsedName
      , paramType = parsedType
      }


commaSeparator :: Parser Prelude.Char
commaSeparator = do
  _ <- Parser.spaces
  comma <- Parser.char ','
  _ <- Parser.spaces
  Parser.yield comma


whereClause :: Parser (Array TypeConstraint)
whereClause =
  Parser.choice
    [ Parser.backtrack do
        _ <- Parser.text "where"
        _ <- Parser.notFollowedBy Parser.alphaNum
        _ <- Parser.whitespace
        typeConstraint |> Parser.separatedOrTerminatedBy commaSeparator
    , Parser.yield Array.empty
    ]
{-# INLINE whereClause #-}


typeConstraint :: Parser TypeConstraint
typeConstraint =
  Parser.choice
    [ Parser.backtrack fieldConstraint
    , Parser.backtrack traitConstraint
    , Parser.backtrack multiParamConstraint
    ]
{-# INLINE typeConstraint #-}


fieldConstraint :: Parser TypeConstraint
fieldConstraint = do
  variableName <- lowerIdentifier
  _ <- Parser.whitespace
  _ <- Parser.text "has"
  _ <- Parser.notFollowedBy Parser.alphaNum
  _ <- Parser.whitespace
  requiredFieldName <- lowerIdentifier
  _ <- Parser.spaces
  _ <- Parser.char ':'
  _ <- Parser.spaces
  requiredFieldType <- captureConstraintType
  Parser.yield
    FieldConstraint
      { variable = variableName
      , fieldName = requiredFieldName
      , fieldType = requiredFieldType
      }


traitConstraint :: Parser TypeConstraint
traitConstraint = do
  variableName <- lowerIdentifier
  _ <- Parser.spaces
  _ <- Parser.char ':'
  _ <- Parser.spaces
  traitNameText <- upperIdentifier
  Parser.yield
    TraitConstraint
      { variable = variableName
      , trait = traitNameText
      }


multiParamConstraint :: Parser TypeConstraint
multiParamConstraint = do
  parsedTraitName <- upperIdentifier
  _ <- Parser.spaces
  _ <- Parser.char '<'
  _ <- Parser.spaces
  parsedTypeArgs <- typeArgument |> Parser.separatedBy commaSeparator
  _ <- Parser.spaces
  _ <- Parser.char '>'
  if parsedTypeArgs |> Array.isEmpty
    then Parser.problem "multi-parameter constraint requires at least one type argument"
    else
      Parser.yield
        MultiParamConstraint
          { traitName = parsedTraitName
          , typeArgs = parsedTypeArgs
          }


typeArgument :: Parser Text
typeArgument = do
  parsedType <- captureTypeUntil typeArgumentTerminator
  ensureNonEmpty "type argument" parsedType


functionBody :: Parser FunctionBody
functionBody =
  Parser.choice
    [ expressionBody
    , blockBody
    ]


expressionBody :: Parser FunctionBody
expressionBody = do
  _ <- Parser.char '='
  _ <- Parser.spaces
  expressionText <- captureExpressionUntil Parser.end
  Parser.yield (Expression {exprText = expressionText})


blockBody :: Parser FunctionBody
blockBody = do
  _ <- Parser.char '{'
  parsedStatements <- blockStatements
  Parser.yield (Block {statements = parsedStatements})


blockStatements :: Parser (Array BlockStatement)
blockStatements = go Array.empty
  where
    go accumulator = do
      _ <- Parser.spaces
      Parser.choice
        [ do
            _ <- Parser.char '}'
            Parser.yield accumulator
        , do
            stmt <- blockStatement
            let newAccumulator = accumulator |> Array.push stmt
            go newAccumulator
        ]


blockStatement :: Parser BlockStatement
blockStatement =
  Parser.choice
    [ Parser.backtrack effectfulBindingStatement
    , Parser.backtrack pureBindingStatement
    , Parser.backtrack returnStatementParser
    , bareExpressionStatement
    ]


effectfulBindingStatement :: Parser BlockStatement
effectfulBindingStatement = do
  _ <- Parser.text "let!"
  _ <- Parser.notFollowedBy Parser.alphaNum
  _ <- Parser.whitespace
  parsedBindingName <- bindingIdentifier
  _ <- Parser.spaces
  _ <- Parser.char '='
  _ <- Parser.spaces
  parsedBindingValue <- captureExpressionUntil statementTerminator
  Parser.yield
    EffectfulBinding
      { bindingName = parsedBindingName
      , bindingValue = parsedBindingValue
      }


pureBindingStatement :: Parser BlockStatement
pureBindingStatement = do
  _ <- Parser.text "let"
  _ <- Parser.notFollowedBy Parser.alphaNum
  _ <- Parser.whitespace
  parsedBindingName <- bindingIdentifier
  _ <- Parser.spaces
  _ <- Parser.char '='
  _ <- Parser.spaces
  parsedBindingValue <- captureExpressionUntil statementTerminator
  Parser.yield
    PureBinding
      { bindingName = parsedBindingName
      , bindingValue = parsedBindingValue
      }


returnStatementParser :: Parser BlockStatement
returnStatementParser = do
  _ <- Parser.text "return"
  _ <- Parser.notFollowedBy Parser.alphaNum
  _ <- Parser.whitespace
  expressionText <- captureExpressionUntil statementTerminator
  Parser.yield (ReturnStatement {exprText = expressionText})


bareExpressionStatement :: Parser BlockStatement
bareExpressionStatement = do
  expressionText <- captureExpressionUntil statementTerminator
  nonEmptyExpression <- ensureNonEmpty "expression statement" expressionText
  Parser.yield (BareExpression {exprText = nonEmptyExpression})


statementTerminator :: Parser Unit
statementTerminator =
  Parser.choice
    [ Parser.newline |> Parser.map (always unit)
    , Parser.char '}' |> Parser.map (always unit)
    ]


typeArgumentTerminator :: Parser Unit
typeArgumentTerminator =
  Parser.choice
    [ Parser.char ',' |> Parser.map (always unit)
    , Parser.char '>' |> Parser.map (always unit)
    ]


captureReturnType :: Parser Text
captureReturnType = do
  parsedType <- captureTypeUntil returnTypeTerminator
  ensureNonEmpty "return type" parsedType


captureParameterType :: Parser Text
captureParameterType = do
  parsedType <- captureTypeUntil parameterTypeTerminator
  ensureNonEmpty "parameter type" parsedType


captureConstraintType :: Parser Text
captureConstraintType = do
  parsedType <- captureTypeUntil constraintTypeTerminator
  ensureNonEmpty "constraint field type" parsedType


captureConstantType :: Parser Text
captureConstantType = do
  parsedType <- captureTypeUntil constantTypeTerminator
  ensureNonEmpty "constant type" parsedType


returnTypeTerminator :: Parser Unit
returnTypeTerminator =
  Parser.choice
    [ Parser.char '{' |> Parser.map (always unit)
    , Parser.char '=' |> Parser.map (always unit)
    , whereKeywordAhead
    , Parser.end
    ]


parameterTypeTerminator :: Parser Unit
parameterTypeTerminator =
  Parser.choice
    [ Parser.char ',' |> Parser.map (always unit)
    , Parser.char ')' |> Parser.map (always unit)
    ]


constraintTypeTerminator :: Parser Unit
constraintTypeTerminator =
  Parser.choice
    [ Parser.char ',' |> Parser.map (always unit)
    , Parser.char '{' |> Parser.map (always unit)
    , Parser.char '=' |> Parser.map (always unit)
    , Parser.end
    ]


constantTypeTerminator :: Parser Unit
constantTypeTerminator =
  Parser.choice
    [ Parser.char '=' |> Parser.map (always unit)
    , Parser.end
    ]


whereKeywordAhead :: Parser Unit
whereKeywordAhead = do
  _ <- Parser.text "where"
  _ <- Parser.notFollowedBy Parser.alphaNum
  Parser.yield unit


ensureNonEmpty :: Text -> Text -> Parser Text
ensureNonEmpty context textValue =
  if textValue |> Text.isEmpty
    then Parser.problem [fmt|expected #{context}|]
    else Parser.yield textValue


captureExpressionUntil :: Parser Unit -> Parser Text
captureExpressionUntil stopParser = go initialExpressionScanState Array.empty
  where
    go scanState accumulator = do
      shouldStopNow <-
        if scanState |> isExpressionTopLevel
          then shouldStop stopParser
          else Parser.yield False
      if shouldStopNow
        then Parser.yield (accumulator |> Text.fromArray |> Text.trim)
        else do
          nextChar <- Parser.anyChar
          let newState = updateExpressionScanState scanState nextChar
          let newAccumulator = accumulator |> Array.push nextChar
          go newState newAccumulator


captureTypeUntil :: Parser Unit -> Parser Text
captureTypeUntil stopParser = go initialTypeScanState Array.empty
  where
    go scanState accumulator = do
      shouldStopNow <-
        if scanState |> isTypeTopLevel
          then shouldStop stopParser
          else Parser.yield False
      if shouldStopNow
        then Parser.yield (accumulator |> Text.fromArray |> Text.trim)
        else do
          nextChar <- Parser.anyChar
          let newState = updateTypeScanState scanState nextChar
          let newAccumulator = accumulator |> Array.push nextChar
          go newState newAccumulator


shouldStop :: Parser Unit -> Parser Bool
shouldStop stopParser =
  Parser.choice
    [ Parser.peek stopParser |> Parser.map (always True)
    , Parser.yield False
    ]
{-# INLINE shouldStop #-}


initialExpressionScanState :: ExpressionScanState
initialExpressionScanState =
  ExpressionScanState
    { parenDepth = 0
    , bracketDepth = 0
    , braceDepth = 0
    , inStringLiteral = False
    , isEscaped = False
    }


isExpressionTopLevel :: ExpressionScanState -> Bool
isExpressionTopLevel scanState =
  scanState.parenDepth == 0
    && scanState.bracketDepth == 0
    && scanState.braceDepth == 0
    && not scanState.inStringLiteral
{-# INLINE isExpressionTopLevel #-}


updateExpressionScanState :: ExpressionScanState -> Prelude.Char -> ExpressionScanState
updateExpressionScanState scanState charValue =
  if scanState.inStringLiteral
    then updateExpressionStringState scanState charValue
    else
      case charValue of
        '"' ->
          scanState
            { inStringLiteral = True
            , isEscaped = False
            }
        '(' ->
          scanState {parenDepth = scanState.parenDepth + 1}
        ')' ->
          scanState {parenDepth = decrementDepth scanState.parenDepth}
        '[' ->
          scanState {bracketDepth = scanState.bracketDepth + 1}
        ']' ->
          scanState {bracketDepth = decrementDepth scanState.bracketDepth}
        '{' ->
          scanState {braceDepth = scanState.braceDepth + 1}
        '}' ->
          scanState {braceDepth = decrementDepth scanState.braceDepth}
        _ ->
          scanState
{-# INLINE updateExpressionScanState #-}


updateExpressionStringState :: ExpressionScanState -> Prelude.Char -> ExpressionScanState
updateExpressionStringState scanState charValue =
  if scanState.isEscaped
    then scanState {isEscaped = False}
    else
      case charValue of
        '\\' ->
          scanState {isEscaped = True}
        '"' ->
          scanState
            { inStringLiteral = False
            , isEscaped = False
            }
        _ ->
          scanState
{-# INLINE updateExpressionStringState #-}


decrementDepth :: Int -> Int
decrementDepth depth = Prelude.max 0 (depth - 1)
{-# INLINE decrementDepth #-}


initialTypeScanState :: TypeScanState
initialTypeScanState =
  TypeScanState
    { typeParenDepth = 0
    , typeBracketDepth = 0
    , typeAngleDepth = 0
    }


isTypeTopLevel :: TypeScanState -> Bool
isTypeTopLevel scanState =
  scanState.typeParenDepth == 0
    && scanState.typeBracketDepth == 0
    && scanState.typeAngleDepth == 0
{-# INLINE isTypeTopLevel #-}


updateTypeScanState :: TypeScanState -> Prelude.Char -> TypeScanState
updateTypeScanState scanState charValue =
  case charValue of
    '(' ->
      scanState {typeParenDepth = scanState.typeParenDepth + 1}
    ')' ->
      scanState {typeParenDepth = decrementDepth scanState.typeParenDepth}
    '[' ->
      scanState {typeBracketDepth = scanState.typeBracketDepth + 1}
    ']' ->
      scanState {typeBracketDepth = decrementDepth scanState.typeBracketDepth}
    '<' ->
      scanState {typeAngleDepth = scanState.typeAngleDepth + 1}
    '>' ->
      scanState {typeAngleDepth = decrementDepth scanState.typeAngleDepth}
    _ ->
      scanState
{-# INLINE updateTypeScanState #-}


-- | Convert a NeoHaskell top-level declaration to Haskell source as a blueprint.
toHaskell :: TopLevelDecl -> Blueprint annotation
toHaskell declaration =
  case declaration of
    FunctionDecl declarationNode ->
      functionDefToHaskell declarationNode
    ConstantDecl declarationNode ->
      constantDefToHaskell declarationNode
{-# INLINE toHaskell #-}


functionDefToHaskell :: FunctionDef -> Blueprint annotation
functionDefToHaskell functionNode = do
  let typeSignatureLine = functionTypeSignatureText functionNode |> Layout.text
  let definitionLine = functionDefinitionBlueprint functionNode
  Layout.stack [typeSignatureLine, definitionLine]


functionTypeSignatureText :: FunctionDef -> Text
functionTypeSignatureText functionNode = do
  let functionName = functionNode.name
  let convertedReturnType = functionNode.returnType |> convertTypeText
  let convertedParamTypes = functionNode.parameters |> Array.map (\param -> param.paramType |> convertTypeText)
  let constraintPrefix = constraintPrefixText functionNode.constraints
  let typeBody =
        if convertedParamTypes |> Array.isEmpty
          then convertedReturnType
          else [fmt|#{convertedParamTypes |> Text.joinWith " -> "} -> #{convertedReturnType}|]
  [fmt|#{functionName} :: #{constraintPrefix}#{typeBody}|]


constraintPrefixText :: Array TypeConstraint -> Text
constraintPrefixText constraintsList =
  if constraintsList |> Array.isEmpty
    then ""
    else do
      let constraintsText = constraintsList |> Array.map constraintToHaskell |> Text.joinWith ", "
      [fmt|(#{constraintsText}) => |]


constraintToHaskell :: TypeConstraint -> Text
constraintToHaskell constraintNode =
  case constraintNode of
    TraitConstraint { variable = variableName, trait = traitNameText } ->
      [fmt|#{traitNameText} #{variableName}|]
    FieldConstraint { variable = variableName, fieldName = requiredFieldName, fieldType = requiredFieldType } -> do
      let convertedFieldType = requiredFieldType |> convertTypeText
      [fmt|HasField "#{requiredFieldName}" #{variableName} #{convertedFieldType}|]
    MultiParamConstraint { traitName = className, typeArgs = arguments } -> do
      let convertedArguments = arguments |> Array.map convertTypeText |> Text.joinWith " "
      if convertedArguments |> Text.isEmpty
        then className
        else [fmt|#{className} #{convertedArguments}|]


functionDefinitionBlueprint :: FunctionDef -> Blueprint annotation
functionDefinitionBlueprint functionNode =
  case functionNode.body of
    Expression { exprText = expressionText } ->
      Layout.text [fmt|#{functionLhsText functionNode} = #{expressionText}|]
    Block { statements = blockStatementsList } -> do
      let statementBlueprints = blockStatementsList |> Array.map blockStatementToBlueprint
      Layout.stack
        [ Layout.text [fmt|#{functionLhsText functionNode} = do|]
        , statementBlueprints |> Layout.stack |> Layout.indent 2
        ]


functionLhsText :: FunctionDef -> Text
functionLhsText functionNode = do
  let functionName = functionNode.name
  let paramsText = functionNode.parameters |> Array.map (.paramName) |> Text.joinWith " "
  if paramsText |> Text.isEmpty
    then functionName
    else [fmt|#{functionName} #{paramsText}|]


blockStatementToBlueprint :: BlockStatement -> Blueprint annotation
blockStatementToBlueprint statementNode =
  case statementNode of
    PureBinding { bindingName = nameText, bindingValue = valueText } ->
      Layout.text [fmt|let #{nameText} = #{valueText}|]
    EffectfulBinding { bindingName = nameText, bindingValue = valueText } ->
      Layout.text [fmt|#{nameText} <- #{valueText}|]
    BareExpression { exprText = expressionText } ->
      Layout.text expressionText
    ReturnStatement { exprText = expressionText } ->
      Layout.text expressionText


constantDefToHaskell :: ConstantDef -> Blueprint annotation
constantDefToHaskell constantNode = do
  let constantName = constantNode.name
  let constantValue = constantNode.value
  let convertedType = constantNode.constType |> convertTypeText
  Layout.stack
    [ Layout.text [fmt|#{constantName} :: #{convertedType}|]
    , Layout.text [fmt|#{constantName} = #{constantValue}|]
    ]


convertTypeText :: Text -> Text
convertTypeText originalType =
  originalType
    |> Text.trim
    |> convertTypeSegment


convertTypeSegment :: Text -> Text
convertTypeSegment typeSegment = do
  let tupleNormalized = typeSegment |> Text.replace "#(" "("
  case tupleNormalized |> findTopLevelAngleOpen of
    Nothing ->
      tupleNormalized
    Just openIndex ->
      case tupleNormalized |> findMatchingAngleClose openIndex of
        Nothing ->
          tupleNormalized
        Just closeIndex -> do
          let prefixText = tupleNormalized |> Text.left openIndex
          let innerText = tupleNormalized |> Text.slice (openIndex + 1) closeIndex
          let suffixText = tupleNormalized |> Text.dropLeft (closeIndex + 1)
          let convertedArguments =
                innerText
                  |> splitTopLevelByComma
                  |> Array.map (Text.trim .> convertTypeSegment .> wrapTypeArgumentIfNeeded)
          let argsText = convertedArguments |> Text.joinWith " "
          let appliedText =
                if argsText |> Text.isEmpty
                  then prefixText
                  else [fmt|#{prefixText} #{argsText}|]
          let convertedSuffix = suffixText |> convertTypeSegment
          [fmt|#{appliedText}#{convertedSuffix}|]


findTopLevelAngleOpen :: Text -> Maybe Int
findTopLevelAngleOpen textValue = go 0 0 0 0 textValue
  where
    go index parens brackets angles remaining =
      case remaining |> Text.uncons of
        Nothing ->
          Nothing
        Just (currentChar, restText) ->
          if currentChar == '<' && parens == 0 && brackets == 0 && angles == 0
            then Just index
            else
              case currentChar of
                '(' -> go (index + 1) (parens + 1) brackets angles restText
                ')' -> go (index + 1) (decrementDepth parens) brackets angles restText
                '[' -> go (index + 1) parens (brackets + 1) angles restText
                ']' -> go (index + 1) parens (decrementDepth brackets) angles restText
                '<' -> go (index + 1) parens brackets (angles + 1) restText
                '>' -> go (index + 1) parens brackets (decrementDepth angles) restText
                _ -> go (index + 1) parens brackets angles restText


findMatchingAngleClose :: Int -> Text -> Maybe Int
findMatchingAngleClose openIndex textValue = go (openIndex + 1) 1 (textValue |> Text.dropLeft (openIndex + 1))
  where
    go :: Int -> Int -> Text -> Maybe Int
    go index depth remaining =
      case remaining |> Text.uncons of
        Nothing ->
          Nothing
        Just (currentChar, restText) ->
          case currentChar of
            '<' ->
              go (index + 1) (depth + 1) restText
            '>' ->
              if depth == 1
                then Just index
                else go (index + 1) (depth - 1) restText
            _ ->
              go (index + 1) depth restText


splitTopLevelByComma :: Text -> Array Text
splitTopLevelByComma textValue =
  if textValue |> Text.trim |> Text.isEmpty
    then Array.empty
    else go textValue 0 0 0 Array.empty Array.empty
  where
    go remaining parens brackets angles parts currentChars =
      case remaining |> Text.uncons of
        Nothing ->
          let part = currentChars |> Text.fromArray |> Text.trim
          in parts |> Array.push part
        Just (currentChar, restText) ->
          if currentChar == ',' && parens == 0 && brackets == 0 && angles == 0
            then do
              let part = currentChars |> Text.fromArray |> Text.trim
              let newParts = parts |> Array.push part
              go restText parens brackets angles newParts Array.empty
            else
              case currentChar of
                '(' ->
                  go restText (parens + 1) brackets angles parts (currentChars |> Array.push currentChar)
                ')' ->
                  go restText (decrementDepth parens) brackets angles parts (currentChars |> Array.push currentChar)
                '[' ->
                  go restText parens (brackets + 1) angles parts (currentChars |> Array.push currentChar)
                ']' ->
                  go restText parens (decrementDepth brackets) angles parts (currentChars |> Array.push currentChar)
                '<' ->
                  go restText parens brackets (angles + 1) parts (currentChars |> Array.push currentChar)
                '>' ->
                  go restText parens brackets (decrementDepth angles) parts (currentChars |> Array.push currentChar)
                _ ->
                  go restText parens brackets angles parts (currentChars |> Array.push currentChar)


wrapTypeArgumentIfNeeded :: Text -> Text
wrapTypeArgumentIfNeeded argumentText = do
  let trimmedArgument = argumentText |> Text.trim
  let alreadyParenthesized =
        (trimmedArgument |> Text.startsWith "(")
          && (trimmedArgument |> Text.endsWith ")")
  if (trimmedArgument |> Text.contains " ") && not alreadyParenthesized
    then [fmt|(#{trimmedArgument})|]
    else trimmedArgument
