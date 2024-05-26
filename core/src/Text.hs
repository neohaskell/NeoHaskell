-- | A built-in representation for efficient string manipulation.
-- @Text@ values are /not/ lists of characters.
module Text (
  -- * Text
  Text,
  isEmpty,
  length,
  reverse,
  repeat,
  replace,

  -- * Building and Splitting
  append,
  concat,
  split,
  join,
  words,
  lines,

  -- * Get Substrings
  slice,
  left,
  right,
  dropLeft,
  dropRight,

  -- * Check for Substrings
  contains,
  startsWith,
  endsWith,
  indexes,
  indices,

  -- * Int Conversions
  toInt,
  fromInt,

  -- * Float Conversions
  toFloat,
  fromFloat,

  -- * Char Conversions
  fromChar,
  cons,
  uncons,

  -- * Array Conversions
  toArray,
  fromArray,

  -- * LinkedList Conversions
  toLinkedList,
  fromLinkedList,

  -- * Random stuff to Text
  tshow,

  -- * Formatting

  -- | Cosmetic operations such as padding with extra characters or trimming whitespace.
  toUpper,
  toLower,
  pad,
  padLeft,
  padRight,
  trim,
  trimLeft,
  trimRight,

  -- * Higher-Order Functions
  map,
  filter,
  foldl,
  foldr,
  any,
  all,
) where

import Array (Array)
import Array qualified
import Basics (Bool, Float, Int, clamp, (+), (-), (.>), (<), (<.), (<=), (|>))
import Char (Char)
import Data.Text qualified
import LinkedList (LinkedList)
import Maybe (Maybe)
import Text.Read qualified
import Prelude (otherwise)
import Prelude qualified


-- | A @Text@ is a chunk of text:
--
-- > "Hello!"
-- > "How are you?"
-- > "🙈🙉🙊"
-- >
-- > -- strings with escape characters
-- > "this\n\t\"that\""
-- > "\x1F648\x1F649\x1F64A" -- "🙈🙉🙊"
--
-- A @Text@ can represent any sequence of [unicode characters](https://en.wikipedia.org/wiki/Unicode). You can use the unicode escapes from @\x0000@ to @\x10FFFF@ to represent characters by their code point. You can also include the unicode characters directly. Using the escapes can be better if you need one of the many whitespace characters with different widths.
type Text = Data.Text.Text


-- | Determine if a string is empty.
--
-- > isEmpty "" == True
-- > isEmpty "the world" == False
isEmpty :: Text -> Bool
isEmpty = Data.Text.null


-- | Get the length of a string.
--
-- > length "innumerable" == 11
-- > length "" == 0
length :: Text -> Int
length =
  Data.Text.length .> Prelude.fromIntegral


-- | Reverse a string.
--
-- > reverse "stressed" == "desserts"
reverse :: Text -> Text
reverse = Data.Text.reverse


-- | Repeat a string /n/ times.
--
-- > repeat 3 "ha" == "hahaha"
repeat :: Int -> Text -> Text
repeat =
  Prelude.fromIntegral .> Data.Text.replicate


-- | Replace all occurrences of some substring.
--
-- > replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"
-- > replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"
replace :: Text -> Text -> Text -> Text
replace = Data.Text.replace


-- BUILDING AND SPLITTING

-- | Append two strings. You can also use the @(++)@ operator to do this.
--
-- > append "butter" "fly" == "butterfly"
append :: Text -> Text -> Text
append = Data.Text.append


-- | Concatenate many strings into one.
--
-- > concat ["never","the","less"] == "nevertheless"
concat :: Array Text -> Text
concat arr =
  Array.toLinkedList arr
    |> Data.Text.concat


-- | Split a string using a given separator.
--
-- > split "," "cat,dog,cow"        == ["cat","dog","cow"]
-- > split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]
split :: Text -> Text -> Array Text
split sep txt =
  Data.Text.splitOn sep txt
    |> Array.fromList


-- | Put many strings together with a given separator.
--
-- > join "a" ["H","w","ii","n"]        == "Hawaiian"
-- > join " " ["cat","dog","cow"]       == "cat dog cow"
-- > join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
join :: Text -> Array Text -> Text
join txt arr =
  Array.toLinkedList arr
    |> Data.Text.intercalate txt


-- | Break a string into words, splitting on chunks of whitespace.
--
-- > words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
words :: Text -> Array Text
words txt =
  Data.Text.words txt
    |> Array.fromList


-- | Break a string into lines, splitting on newlines.
--
-- > lines "How are you?\nGood?" == ["How are you?", "Good?"]
lines :: Text -> Array Text
lines txt =
  Data.Text.lines txt
    |> Array.fromList


-- SUBSTRINGS

-- | Take a substring given a start and end index. Negative indexes
-- are taken starting from the /end/ of the list.
--
-- > slice  7  9 "snakes on a plane!" == "on"
-- > slice  0  6 "snakes on a plane!" == "snakes"
-- > slice  0 -7 "snakes on a plane!" == "snakes on a"
-- > slice -6 -1 "snakes on a plane!" == "plane"
slice :: Int -> Int -> Text -> Text
slice from to text
  | to' - from' <= 0 = Data.Text.empty
  | otherwise =
      Data.Text.drop from' (Data.Text.take to' text)
 where
  len = Data.Text.length text
  handleNegative value
    | value < 0 = len + value
    | otherwise = value
  normalize =
    Prelude.fromIntegral
      .> handleNegative
      .> clamp 0 len
  from' = normalize from
  to' = normalize to


-- | Take /n/ characters from the left side of a string.
--
-- > left 2 "Mulder" == "Mu"
left :: Int -> Text -> Text
left =
  Prelude.fromIntegral .> Data.Text.take


-- | Take /n/ characters from the right side of a string.
--
-- > right 2 "Scully" == "ly"
right :: Int -> Text -> Text
right =
  Prelude.fromIntegral .> Data.Text.takeEnd


-- | Drop /n/ characters from the left side of a string.
--
-- > dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"
dropLeft :: Int -> Text -> Text
dropLeft =
  Prelude.fromIntegral .> Data.Text.drop


-- | Drop /n/ characters from the right side of a string.
--
-- > dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"
dropRight :: Int -> Text -> Text
dropRight =
  Prelude.fromIntegral .> Data.Text.dropEnd


-- DETECT SUBSTRINGS

-- | See if the second string contains the first one.
--
-- > contains "the" "theory" == True
-- > contains "hat" "theory" == False
-- > contains "THE" "theory" == False
contains :: Text -> Text -> Bool
contains = Data.Text.isInfixOf


-- | See if the second string starts with the first one.
--
-- > startsWith "the" "theory" == True
-- > startsWith "ory" "theory" == False
startsWith :: Text -> Text -> Bool
startsWith = Data.Text.isPrefixOf


-- | See if the second string ends with the first one.
--
-- > endsWith "the" "theory" == False
-- > endsWith "ory" "theory" == True
endsWith :: Text -> Text -> Bool
endsWith = Data.Text.isSuffixOf


-- | Get all of the indexes for a substring in another string.
--
-- > indexes "i" "Mississippi"   == [1,4,7,10]
-- > indexes "ss" "Mississippi"  == [2,5]
-- > indexes "needle" "haystack" == []
indexes :: Text -> Text -> Array Int
indexes n h
  | isEmpty n = Array.empty
  | otherwise = indexes' n h
 where
  indexes' needle haystack =
    Data.Text.breakOnAll needle haystack
      |> Array.fromList
      |> Array.map
        ( \(lhs, _) ->
            Data.Text.length lhs
              |> Prelude.fromIntegral
        )


-- | Alias for @indexes@.
indices :: Text -> Text -> Array Int
indices = indexes


-- FORMATTING

-- | Convert a string to all upper case. Useful for case-insensitive comparisons
-- and VIRTUAL YELLING.
--
-- > toUpper "skinner" == "SKINNER"
toUpper :: Text -> Text
toUpper = Data.Text.toUpper


-- | Convert a string to all lower case. Useful for case-insensitive comparisons.
--
-- > toLower "X-FILES" == "x-files"
toLower :: Text -> Text
toLower = Data.Text.toLower


-- | Pad a string on both sides until it has a given length.
--
-- > pad 5 ' ' "1"   == "  1  "
-- > pad 5 ' ' "11"  == "  11 "
-- > pad 5 ' ' "121" == " 121 "
pad :: Int -> Char -> Text -> Text
pad =
  Prelude.fromIntegral .> Data.Text.center


-- | Pad a string on the left until it has a given length.
--
-- > padLeft 5 '.' "1"   == "....1"
-- > padLeft 5 '.' "11"  == "...11"
-- > padLeft 5 '.' "121" == "..121"
padLeft :: Int -> Char -> Text -> Text
padLeft =
  Prelude.fromIntegral .> Data.Text.justifyRight


-- | Pad a string on the right until it has a given length.
--
-- > padRight 5 '.' "1"   == "1...."
-- > padRight 5 '.' "11"  == "11..."
-- > padRight 5 '.' "121" == "121.."
padRight :: Int -> Char -> Text -> Text
padRight =
  Prelude.fromIntegral .> Data.Text.justifyLeft


-- | Get rid of whitespace on both sides of a string.
--
-- > trim "  hats  \n" == "hats"
trim :: Text -> Text
trim = Data.Text.strip


-- | Get rid of whitespace on the left of a string.
--
-- > trimLeft "  hats  \n" == "hats  \n"
trimLeft :: Text -> Text
trimLeft = Data.Text.stripStart


-- | Get rid of whitespace on the right of a string.
--
-- > trimRight "  hats  \n" == "  hats"
trimRight :: Text -> Text
trimRight = Data.Text.stripEnd


-- INT CONVERSIONS

-- | Try to convert a string into an int, failing on improperly formatted strings.
--
-- > Text.toInt "123" == Just 123
-- > Text.toInt "-42" == Just -42
-- > Text.toInt "3.1" == Nothing
-- > Text.toInt "31a" == Nothing
--
-- If you are extracting a number from some raw user input, you will typically
-- want to use [@Maybe.withDefault@](Maybe#withDefault) to handle bad data:
--
-- > Maybe.withDefault 0 (Text.toInt "42") == 42
-- > Maybe.withDefault 0 (Text.toInt "ab") == 0
toInt :: Text -> Maybe Int
toInt text =
  Text.Read.readMaybe str'
 where
  str = Data.Text.unpack text
  str' = case str of
    '+' : rest -> rest
    other -> other


-- | Convert an @Int@ to a @Text@.
--
-- > Text.fromInt 123 == "123"
-- > Text.fromInt -42 == "-42"
fromInt :: Int -> Text
fromInt = Data.Text.pack <. Prelude.show


-- FLOAT CONVERSIONS

-- | Try to convert a string into a float, failing on improperly formatted strings.
--
-- > Text.toFloat "123" == Just 123.0
-- > Text.toFloat "-42" == Just -42.0
-- > Text.toFloat "3.1" == Just 3.1
-- > Text.toFloat "31a" == Nothing
--
-- If you are extracting a number from some raw user input, you will typically
-- want to use [@Maybe.withDefault@](Maybe#withDefault) to handle bad data:
--
-- > Maybe.withDefault 0 (Text.toFloat "42.5") == 42.5
-- > Maybe.withDefault 0 (Text.toFloat "cats") == 0
toFloat :: Text -> Maybe Float
toFloat text =
  Text.Read.readMaybe str'
 where
  str = Data.Text.unpack text
  str' = case str of
    '+' : rest -> rest
    '.' : rest -> '0' : '.' : rest
    other -> other


-- | Convert a @Float@ to a @Text@.
--
-- > Text.fromFloat 123 == "123"
-- > Text.fromFloat -42 == "-42"
-- > Text.fromFloat 3.9 == "3.9"
fromFloat :: Float -> Text
fromFloat = Data.Text.pack <. Prelude.show


-- LIST CONVERSIONS

-- | Convert a Text to a list of characters.
--
-- > toArray "abc" == ['a','b','c']
-- > toArray "🙈🙉🙊" == ['🙈','🙉','🙊']
toArray :: Text -> Array Char
toArray txt =
  Data.Text.unpack txt
    |> Array.fromList


-- | Convert a list of characters into a Text. Can be useful if you
-- want to create a string primarily by consing, perhaps for decoding
-- something.
--
-- > fromArray ['a','b','c'] == "abc"
-- > fromArray ['🙈','🙉','🙊'] == "🙈🙉🙊"
fromArray :: Array Char -> Text
fromArray arr =
  Array.toLinkedList arr
    |> Data.Text.pack


-- LINKED LIST CONVERSIONS

-- | Convert a Text to a linked list of characters.
toLinkedList :: Text -> LinkedList Char
toLinkedList txt =
  Data.Text.unpack txt


-- | Convert a linked list of characters into a Text.
fromLinkedList :: LinkedList Char -> Text
fromLinkedList list = Data.Text.pack list


-- CHAR CONVERSIONS

-- | Create a Text from a given character.
--
-- > fromChar 'a' == "a"
fromChar :: Char -> Text
fromChar = Data.Text.singleton


-- | Add a character to the beginning of a Text.
--
-- > cons 'T' "he truth is out there" == "The truth is out there"
cons :: Char -> Text -> Text
cons = Data.Text.cons


-- | Split a non-empty Text into its head and tail. This lets you
-- pattern match on strings exactly as you would with lists.
--
-- > uncons "abc" == Just ('a',"bc")
-- > uncons ""    == Nothing
uncons :: Text -> Maybe (Char, Text)
uncons = Data.Text.uncons


-- HIGHER-ORDER FUNCTIONS

-- | Transform every character in a Text
--
-- > map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
map :: (Char -> Char) -> Text -> Text
map = Data.Text.map


-- | Keep only the characters that pass the test.
--
-- > filter isDigit "R2-D2" == "22"
filter :: (Char -> Bool) -> Text -> Text
filter = Data.Text.filter


-- | Reduce a Text from the left.
--
-- > foldl cons "" "time" == "emit"
foldl :: (Char -> b -> b) -> b -> Text -> b
foldl f = Data.Text.foldl' (\a b -> f b a)


-- | Reduce a Text from the right.
--
-- > foldr cons "" "time" == "time"
foldr :: (Char -> b -> b) -> b -> Text -> b
foldr = Data.Text.foldr


-- | Determine whether /any/ characters pass the test.
--
-- > any isDigit "90210" == True
-- > any isDigit "R2-D2" == True
-- > any isDigit "heart" == False
any :: (Char -> Bool) -> Text -> Bool
any = Data.Text.any


-- | Determine whether /all/ characters pass the test.
--
-- > all isDigit "90210" == True
-- > all isDigit "R2-D2" == False
-- > all isDigit "heart" == False
all :: (Char -> Bool) -> Text -> Bool
all = Data.Text.all


-- | Get a Text representation of something Show-able.
--
-- > newtype MyType = MyType deriving (Show)
-- > myTypeText = tshow MyType
tshow :: (Prelude.Show a) => a -> Text
tshow = Data.Text.pack <. Prelude.show
