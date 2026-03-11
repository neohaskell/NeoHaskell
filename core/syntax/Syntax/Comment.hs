-- | NeoHaskell comment syntax parsing and transpilation.
--
-- Parsers that __capture__ comment content (unlike 'Parser.lineComment' and
-- friends which silently skip). Use these for transpilation, documentation
-- extraction, and IDE hover providers.
--
-- == Quick Start
--
-- @
-- import Syntax.Comment (Comment (..))
-- import Syntax.Comment qualified
-- import Parser qualified
--
-- result :: Result ParseError Comment
-- result = "\/\/ hello world" |> Parser.run Syntax.Comment.lineComment
-- @
--
-- == Transpilation
--
-- @
-- comment |> Syntax.Comment.toHaskell
-- @
--
-- Produces valid Haskell comment syntax:
--
-- * @\/\/ content@  → @-- content@
-- * @\/* content *\/@  → @{- content -}@  (nesting preserved)
-- * @\/** content *\/@  → @-- | content@  (Haddock)
module Syntax.Comment
  ( -- * Types
    Comment (..)
    -- * Parsers
  , lineComment
  , blockComment
  , docComment
  , comment
    -- * Transpilation
  , toHaskell
  ) where

import Basics
import Array qualified
import Parser (Parser, ParsePosition (..))
import Parser qualified
import Text (Text)
import Text qualified
import Prelude qualified


-- | A NeoHaskell source comment, captured with its content and source position.
--
-- The three constructors correspond to the three comment syntaxes defined in
-- the NeoHaskell specification (§7):
--
-- * 'LineComment' for @\/\/ content@ (single-line)
-- * 'BlockComment' for @\/* content *\/@ (block, correct nesting supported)
-- * 'DocComment' for @\/** content *\/@ (documentation, Haddock target)
--
-- Obtain a 'Comment' by running one of the parsers in this module:
--
-- @
-- import Syntax.Comment (Comment)
-- import Syntax.Comment qualified
-- import Parser qualified
--
-- result :: Result ParseError Comment
-- result = "\/\/ hello world" |> Parser.run Syntax.Comment.lineComment
-- @
data Comment
  = LineComment
      { position :: ParsePosition
        -- ^ Start position of the @\/\/@ token (1-based line and column).
      , content  :: Text
        -- ^ Text from after @\/\/ @ to end of line (one leading space stripped if present).
      }
  | BlockComment
      { position :: ParsePosition
        -- ^ Start position of the opening @\/*@ token.
      , content  :: Text
        -- ^ Raw inner text, including any nested @\/* ... *\/@ markers verbatim.
        -- 'toHaskell' substitutes these to produce valid Haskell @{- ... -}@ nesting.
      , depth    :: {-# UNPACK #-} Int
        -- ^ Maximum nesting depth observed during parsing.
        -- @1@ = flat block comment (no inner @\/*@).
        -- @2+@ = contains nested block comments.
      }
  | DocComment
      { position :: ParsePosition
        -- ^ Start position of the opening @\/**@ token.
      , content  :: Text
        -- ^ Inner text. Leading @* @ decoration is stripped from each line.
        -- The opening @**@ is not included.
      }
  deriving (Eq, Prelude.Show, Generic)


-- | Parse a NeoHaskell line comment (@\/\/ ...@) and capture its content.
--
-- Consumes from @\/\/@ to (but not including) the newline character.
-- One leading space after @\/\/@ is stripped from the captured content.
--
-- __Note__: This CAPTURES comment text. To skip a line comment without capturing,
-- use @Parser.lineComment \"\/\/\"@ from the general-purpose 'Parser' module.
--
-- @
-- "\/\/ hello world\\n" |> Parser.run Syntax.Comment.lineComment
-- -- Ok (LineComment { content = "hello world", ... })
-- @
lineComment :: Parser Comment
lineComment = do
  pos <- Parser.position
  _ <- Parser.text "//"
  _ <- Parser.optional (Parser.char ' ')
  chars <- Parser.anyCharExcept '\n' |> Parser.zeroOrMore
  let capturedContent = chars |> Text.fromArray
  Parser.yield (LineComment { position = pos, content = capturedContent })
{-# INLINE lineComment #-}


-- | Parse a NeoHaskell block comment (@\/* ... *\/@) and capture its content.
--
-- Supports nesting: @\/* outer \/* inner *\/ outer *\/@ is valid and the
-- inner @\/* *\/@ markers are preserved verbatim in 'BlockComment.content'.
-- The 'BlockComment.depth' field records the maximum nesting depth seen.
--
-- __Note__: This CAPTURES comment text. To skip a block comment, use
-- @Parser.blockCommentNested \"\/*\" \"*\/\"@ from the 'Parser' module.
blockComment :: Parser Comment
blockComment = do
  pos <- Parser.position
  _ <- Parser.text "/*"
  (capturedContent, maxDepth) <- go 1 1 ""
  Parser.yield
    BlockComment
      { position = pos
      , content  = capturedContent
      , depth    = maxDepth
      }
  where
    go currentDepth maxSeen accumulator =
      Parser.choice
        [ do
            _ <- Parser.text "*/"
            if currentDepth == 1
              then Parser.yield (accumulator, maxSeen)
              else do
                let newContent = [fmt|#{accumulator}*/|]
                go (currentDepth - 1) maxSeen newContent
        , do
            _ <- Parser.text "/*"
            let newDepth = currentDepth + 1
            let newMax = Prelude.max newDepth maxSeen
            let newContent = [fmt|#{accumulator}/*|]
            go newDepth newMax newContent
        , do
            ch <- Parser.anyChar
            let newContent = [fmt|#{accumulator}#{ch}|]
            go currentDepth maxSeen newContent
        ]


-- | Parse a NeoHaskell doc comment (@\/** ... *\/@) and capture its content.
--
-- Doc comments attach to the declaration immediately following them and are
-- extracted by documentation tooling and IDE hover providers.
-- Leading @* @ decoration on each interior line is stripped from the content.
--
-- @
-- "\/** Calculates distance. *\/" |> Parser.run Syntax.Comment.docComment
-- -- Ok (DocComment { content = "Calculates distance.", ... })
-- @
docComment :: Parser Comment
docComment = do
  pos <- Parser.position
  _ <- Parser.text "/**"
  chars <- Parser.anyChar |> Parser.collectUntil (Parser.text "*/")
  let rawContent = chars |> Text.fromArray
  let capturedContent =
        rawContent
          |> Text.lines
          |> Array.map stripDecoration
          |> Text.joinWith "\n"
          |> Text.trim
  Parser.yield (DocComment { position = pos, content = capturedContent })
{-# INLINE docComment #-}


-- | Strip leading @* @ decoration from a doc comment line.
stripDecoration :: Text -> Text
stripDecoration line = do
  let trimmed = line |> Text.trim
  if trimmed |> Text.startsWith "* "
    then trimmed |> Text.dropLeft 2
    else if trimmed == "*"
      then ""
      else trimmed


-- | Parse any NeoHaskell comment (doc, block, or line — in that order).
--
-- 'docComment' must be tried before 'blockComment' because @\/**@ starts with @\/*@.
-- Both are wrapped in 'Parser.backtrack' so a failed attempt does not consume input.
--
-- @
-- import Syntax.Comment qualified
-- import Parser qualified
--
-- parseComments :: Parser (Array Comment)
-- parseComments =
--   Syntax.Comment.comment |> Parser.zeroOrMore
-- @
comment :: Parser Comment
comment =
  Parser.choice
    [ docComment
    , blockComment
    , lineComment
    ]


-- | Convert a NeoHaskell 'Comment' to its Haskell equivalent.
--
-- Transpilation rules (NeoHaskell syntax spec §7):
--
-- * 'LineComment' @\/\/ content@ → @-- content@
-- * 'BlockComment' @\/* content *\/@ → @{- content -}@  (nesting preserved)
-- * 'DocComment' @\/** content *\/@ → @-- | content@  (Haddock format)
--
-- __Nesting__: nested @\/* ... *\/@ markers inside 'BlockComment.content' are
-- replaced with @{- ... -}@ to produce valid GHC nested block comments.
--
-- __Multi-line doc comments__: each content line becomes a separate @-- |@ line.
--
-- @
-- toHaskell (LineComment { content = "hello", ... })
--   == "-- hello"
--
-- toHaskell (BlockComment { content = " world ", depth = 1, ... })
--   == "{- world -}"
--
-- toHaskell (DocComment { content = "Calculates distance.", ... })
--   == "-- | Calculates distance."
-- @
toHaskell :: Comment -> Text
toHaskell commentNode =
  case commentNode of
    LineComment { content = capturedContent } ->
      [fmt|-- #{capturedContent}|]
    BlockComment { content = capturedContent } -> do
      let innerHaskell =
            capturedContent
              |> Text.replace "/*" "{-"
              |> Text.replace "*/" "-}"
      [fmt|{-#{innerHaskell}-}|]
    DocComment { content = capturedContent } -> do
      let contentLines = capturedContent |> Text.lines
      case Array.length contentLines of
        0 ->
          "-- |"
        _ ->
          contentLines
            |> Array.map (\line -> [fmt|-- | #{line}|])
            |> Text.joinWith "\n"
