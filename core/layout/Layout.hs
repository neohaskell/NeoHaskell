module Layout (
  -- * Types
  Blueprint,
  RenderOptions,
  WrapMode (..),

  -- * Typeclass
  ToBlueprint (..),

  -- * Core Construction
  empty,
  text,
  value,
  space,
  line,
  lineTight,
  softLine,
  softLineTight,
  hardLine,
  blankLine,

  -- * Composition and Joining
  append,
  appendWithSpace,
  joinWith,
  joinWords,
  stack,
  joinParagraph,
  joinAdaptive,
  joinTight,
  stackTight,
  joinDenseParagraph,
  joinAdaptiveTight,
  addBetween,
  commaSeparated,
  semicolonSeparated,
  pipeSeparated,

  -- * Delimiters and Wrapping
  wrapBetween,
  asList,
  asTuple,
  inParens,
  inBrackets,
  inBraces,
  inAngles,
  inSingleQuotes,
  inDoubleQuotes,

  -- * Layout Control
  trySingleLine,
  chooseWhenSingleLine,
  indent,
  indentRelative,
  alignToCurrentColumn,
  hangingIndent,
  padToWidth,
  padOrBreakAt,
  mergeAdjacentBreaks,

  -- * Annotations
  withAnnotation,
  withoutAnnotations,
  mapAnnotations,
  editAnnotations,

  -- * Rendering Options
  defaultRenderOptions,
  withMaxWidth,
  withRibbonFraction,
  withWrapMode,

  -- * Rendering
  render,
  renderWithWidth,
  renderWith,
  renderBalanced,
  renderFast,
  renderCompact,
) where

import Array (Array)
import Array qualified
import Basics
import LinkedList qualified
import Text (Text)
import Text qualified

import Prettyprinter qualified as GhcPretty

import Layout.Internal (
  Blueprint,
  RenderOptions (..),
  WrapMode (..),
  ToBlueprint (..),
  wrap,
  unwrap,
  )
import Layout.Internal qualified as Internal
import Layout.Internal.Render qualified as Render


empty :: Blueprint annotation
empty = Internal.wrap GhcPretty.emptyDoc
{-# INLINE empty #-}


text :: Text -> Blueprint annotation
text t = wrap (GhcPretty.pretty t)
{-# INLINE text #-}


value :: forall val annotation. (ToBlueprint val) => val -> Blueprint annotation
value v = toBlueprint v
{-# INLINE value #-}


space :: Blueprint annotation
space = wrap GhcPretty.space
{-# INLINE space #-}


line :: Blueprint annotation
line = wrap GhcPretty.line
{-# INLINE line #-}


lineTight :: Blueprint annotation
lineTight = wrap GhcPretty.line'
{-# INLINE lineTight #-}


softLine :: Blueprint annotation
softLine = wrap GhcPretty.softline
{-# INLINE softLine #-}


softLineTight :: Blueprint annotation
softLineTight = wrap GhcPretty.softline'
{-# INLINE softLineTight #-}


hardLine :: Blueprint annotation
hardLine = wrap GhcPretty.hardline
{-# INLINE hardLine #-}


blankLine :: Blueprint annotation
blankLine = wrap (GhcPretty.hardline GhcPretty.<> GhcPretty.hardline)
{-# INLINE blankLine #-}


append :: Blueprint ann -> Blueprint ann -> Blueprint ann
append right left = Internal.wrap (Internal.unwrap left GhcPretty.<> Internal.unwrap right)
{-# INLINE append #-}


appendWithSpace :: Blueprint ann -> Blueprint ann -> Blueprint ann
appendWithSpace right left = wrap (unwrap left GhcPretty.<+> unwrap right)
{-# INLINE appendWithSpace #-}


joinWith :: Blueprint ann -> Array (Blueprint ann) -> Blueprint ann
joinWith sep items =
  wrap
    ( GhcPretty.concatWith
        (\a b -> a GhcPretty.<> unwrap sep GhcPretty.<> b)
        (items |> Array.map unwrap |> Array.toLinkedList)
    )
{-# INLINE joinWith #-}


joinWords :: Array (Blueprint ann) -> Blueprint ann
joinWords items = wrap (GhcPretty.hsep (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinWords #-}


stack :: Array (Blueprint ann) -> Blueprint ann
stack items = wrap (GhcPretty.vsep (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE stack #-}


joinParagraph :: Array (Blueprint ann) -> Blueprint ann
joinParagraph items = wrap (GhcPretty.fillSep (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinParagraph #-}


joinAdaptive :: Array (Blueprint ann) -> Blueprint ann
joinAdaptive items = wrap (GhcPretty.sep (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinAdaptive #-}


joinTight :: Array (Blueprint ann) -> Blueprint ann
joinTight items = wrap (GhcPretty.hcat (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinTight #-}


stackTight :: Array (Blueprint ann) -> Blueprint ann
stackTight items = wrap (GhcPretty.vcat (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE stackTight #-}


joinDenseParagraph :: Array (Blueprint ann) -> Blueprint ann
joinDenseParagraph items = wrap (GhcPretty.fillCat (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinDenseParagraph #-}


joinAdaptiveTight :: Array (Blueprint ann) -> Blueprint ann
joinAdaptiveTight items = wrap (GhcPretty.cat (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE joinAdaptiveTight #-}


addBetween :: Blueprint ann -> Array (Blueprint ann) -> Array (Blueprint ann)
addBetween sep items =
  GhcPretty.punctuate (unwrap sep) (items |> Array.map unwrap |> Array.toLinkedList)
    |> LinkedList.map wrap
    |> Array.fromLinkedList
{-# INLINE addBetween #-}


commaSeparated :: Array (Blueprint ann) -> Blueprint ann
commaSeparated items = items |> addBetween (text ", ") |> joinTight
{-# INLINE commaSeparated #-}


semicolonSeparated :: Array (Blueprint ann) -> Blueprint ann
semicolonSeparated items = items |> addBetween (text "; ") |> joinTight
{-# INLINE semicolonSeparated #-}


pipeSeparated :: Array (Blueprint ann) -> Blueprint ann
pipeSeparated items = items |> addBetween (text " | ") |> joinTight
{-# INLINE pipeSeparated #-}


wrapBetween :: Blueprint ann -> Blueprint ann -> Blueprint ann -> Blueprint ann
wrapBetween open close content = wrap (GhcPretty.enclose (unwrap open) (unwrap close) (unwrap content))
{-# INLINE wrapBetween #-}


asList :: Array (Blueprint ann) -> Blueprint ann
asList items = wrap (GhcPretty.list (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE asList #-}


asTuple :: Array (Blueprint ann) -> Blueprint ann
asTuple items = wrap (GhcPretty.tupled (items |> Array.map unwrap |> Array.toLinkedList))
{-# INLINE asTuple #-}


inParens :: Blueprint ann -> Blueprint ann
inParens x = wrap (GhcPretty.parens (unwrap x))
{-# INLINE inParens #-}


inBrackets :: Blueprint ann -> Blueprint ann
inBrackets x = wrap (GhcPretty.brackets (unwrap x))
{-# INLINE inBrackets #-}


inBraces :: Blueprint ann -> Blueprint ann
inBraces x = wrap (GhcPretty.braces (unwrap x))
{-# INLINE inBraces #-}


inAngles :: Blueprint ann -> Blueprint ann
inAngles x = wrap (GhcPretty.angles (unwrap x))
{-# INLINE inAngles #-}


inSingleQuotes :: Blueprint ann -> Blueprint ann
inSingleQuotes x = wrap (GhcPretty.squotes (unwrap x))
{-# INLINE inSingleQuotes #-}


inDoubleQuotes :: Blueprint ann -> Blueprint ann
inDoubleQuotes x = wrap (GhcPretty.dquotes (unwrap x))
{-# INLINE inDoubleQuotes #-}


trySingleLine :: Blueprint ann -> Blueprint ann
trySingleLine x = wrap (GhcPretty.group (unwrap x))
{-# INLINE trySingleLine #-}


chooseWhenSingleLine :: Blueprint ann -> Blueprint ann -> Blueprint ann
chooseWhenSingleLine flat broken = wrap (GhcPretty.flatAlt (unwrap broken) (unwrap flat))
{-# INLINE chooseWhenSingleLine #-}


indent :: Int -> Blueprint ann -> Blueprint ann
indent n x =
  let clamped = clamp 0 10_000 n
  in wrap (GhcPretty.nest clamped (unwrap x))
{-# INLINE indent #-}


indentRelative :: Int -> Blueprint ann -> Blueprint ann
indentRelative n x =
  let clamped = clamp 0 10_000 n
  in wrap (GhcPretty.indent clamped (unwrap x))
{-# INLINE indentRelative #-}


alignToCurrentColumn :: Blueprint ann -> Blueprint ann
alignToCurrentColumn x = wrap (GhcPretty.align (unwrap x))
{-# INLINE alignToCurrentColumn #-}


hangingIndent :: Int -> Blueprint ann -> Blueprint ann
hangingIndent n x =
  let clamped = clamp 0 10_000 n
  in wrap (GhcPretty.hang clamped (unwrap x))
{-# INLINE hangingIndent #-}


padToWidth :: Int -> Blueprint ann -> Blueprint ann
padToWidth n x =
  let clamped = clamp 0 10_000 n
  in wrap (GhcPretty.fill clamped (unwrap x))
{-# INLINE padToWidth #-}


padOrBreakAt :: Int -> Blueprint ann -> Blueprint ann
padOrBreakAt n x =
  let clamped = clamp 0 10_000 n
      brokenWidth = clamp 0 10_000 (clamped - 1)
      doc = unwrap x
  in
    wrap
      ( GhcPretty.width doc
          ( \docWidth ->
              if docWidth > clamped
                then GhcPretty.nest brokenWidth GhcPretty.line'
                else GhcPretty.pretty (Text.repeat (clamped - docWidth) " ")
          )
      )
{-# INLINE padOrBreakAt #-}


mergeAdjacentBreaks :: Int -> Blueprint ann -> Blueprint ann
mergeAdjacentBreaks depth x =
  let clamped = clamp 0 10_000 depth
      fusionDepth =
        case clamped of
          0 -> GhcPretty.Shallow
          _ -> GhcPretty.Deep
  in wrap (GhcPretty.fuse fusionDepth (unwrap x))
{-# INLINE mergeAdjacentBreaks #-}


withAnnotation :: ann -> Blueprint ann -> Blueprint ann
withAnnotation ann x = wrap (GhcPretty.annotate ann (unwrap x))
{-# INLINE withAnnotation #-}


withoutAnnotations :: Blueprint ann -> Blueprint ignored
withoutAnnotations x = wrap (GhcPretty.unAnnotate (unwrap x))
{-# INLINE withoutAnnotations #-}


mapAnnotations :: forall ann newAnn. (ann -> newAnn) -> Blueprint ann -> Blueprint newAnn
mapAnnotations f x = wrap (GhcPretty.reAnnotate f (unwrap x))
{-# INLINE mapAnnotations #-}


editAnnotations :: forall ann newAnn. (ann -> Array newAnn) -> Blueprint ann -> Blueprint newAnn
editAnnotations f x = wrap (GhcPretty.alterAnnotations (\ann -> ann |> f |> Array.toLinkedList) (unwrap x))
{-# INLINE editAnnotations #-}


defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions {maxWidth = 80, ribbonFraction = 1.0, wrapMode = Balanced}


withMaxWidth :: Int -> RenderOptions -> RenderOptions
withMaxWidth n opts = opts {maxWidth = clamp 0 10_000 n}
{-# INLINE withMaxWidth #-}


withRibbonFraction :: Float -> RenderOptions -> RenderOptions
withRibbonFraction f opts =
  let clamped =
        if isNaN f || isInfinite f || f <= 0.0 || f > 1.0
          then 1.0
          else f
  in opts {ribbonFraction = clamped}
{-# INLINE withRibbonFraction #-}


withWrapMode :: WrapMode -> RenderOptions -> RenderOptions
withWrapMode mode opts = opts {wrapMode = mode}
{-# INLINE withWrapMode #-}


render :: Blueprint ann -> Text
render bp = Render.renderWith defaultRenderOptions bp
{-# INLINE render #-}


renderWithWidth :: Int -> Blueprint ann -> Text
renderWithWidth n bp = Render.renderWith (defaultRenderOptions |> withMaxWidth n) bp
{-# INLINE renderWithWidth #-}


renderWith :: RenderOptions -> Blueprint ann -> Text
renderWith opts bp = Render.renderWith opts bp
{-# INLINE renderWith #-}


renderBalanced :: Blueprint ann -> Text
renderBalanced bp = Render.renderWith (defaultRenderOptions |> withWrapMode Balanced) bp
{-# INLINE renderBalanced #-}


renderFast :: Blueprint ann -> Text
renderFast bp = Render.renderWith (defaultRenderOptions |> withWrapMode Fast) bp
{-# INLINE renderFast #-}


renderCompact :: Blueprint ann -> Text
renderCompact bp = Render.renderWith (defaultRenderOptions |> withWrapMode Compact) bp
{-# INLINE renderCompact #-}