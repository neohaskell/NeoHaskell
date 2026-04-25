{-# OPTIONS_GHC -Wno-unused-imports #-}

module TextSpec where

import Array qualified
import Core
import LinkedList qualified
import Maybe qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Text" do
    describe "Basic Properties" do
      describe "isEmpty" do
        it "returns True for empty text" \_ -> do
          "" |> Text.isEmpty |> shouldBe True

        it "returns False for non-empty text" \_ -> do
          "the world" |> Text.isEmpty |> shouldBe False

      describe "length" do
        it "returns 0 for empty text" \_ -> do
          "" |> Text.length |> shouldBe 0

        it "returns the number of characters" \_ -> do
          "innumerable" |> Text.length |> shouldBe 11
          "🙈🙉🙊" |> Text.length |> shouldBe 3

      describe "reverse" do
        it "reverses the text" \_ -> do
          "stressed" |> Text.reverse |> shouldBe "desserts"

        it "handles empty text" \_ -> do
          "" |> Text.reverse |> shouldBe ""

      describe "repeat" do
        it "repeats a text n times" \_ -> do
          "ha" |> Text.repeat 3 |> shouldBe "hahaha"

        it "returns empty text when repeated 0 times" \_ -> do
          "ha" |> Text.repeat 0 |> shouldBe ""

      describe "replace" do
        it "replaces all occurrences of a substring" \_ -> do
          "Json.Decode.succeed" |> Text.replace "." "-" |> shouldBe "Json-Decode-succeed"
          "a,b,c,d,e" |> Text.replace "," "/" |> shouldBe "a/b/c/d/e"

    describe "Building and Splitting" do
      describe "append" do
        it "appends two texts" \_ -> do
          "butterfly" |> shouldBe ("butter" |> Text.append "fly")

      describe "concat" do
        it "concatenates an array of texts" \_ -> do
          ["never", "the", "less"] |> Array.fromLinkedList |> Text.concat |> shouldBe "nevertheless"

      describe "split" do
        it "splits text on a separator" \_ -> do
          "cat,dog,cow" |> Text.split "," |> shouldBe (Array.fromLinkedList ["cat", "dog", "cow"])
          "home/evan/Desktop/" |> Text.split "/" |> shouldBe (Array.fromLinkedList ["home", "evan", "Desktop", ""])

      describe "joinWith" do
        it "joins texts with a separator" \_ -> do
          ["H", "w", "ii", "n"] |> Array.fromLinkedList |> Text.joinWith "a" |> shouldBe "Hawaiian"
          ["cat", "dog", "cow"] |> Array.fromLinkedList |> Text.joinWith " " |> shouldBe "cat dog cow"

      describe "words" do
        it "breaks text into words" \_ -> do
          "How are \t you? \n Good?" |> Text.words |> shouldBe (Array.fromLinkedList ["How", "are", "you?", "Good?"])

      describe "lines" do
        it "breaks text into lines" \_ -> do
          "How are you?\nGood?" |> Text.lines |> shouldBe (Array.fromLinkedList ["How are you?", "Good?"])

    describe "Substrings" do
      describe "slice" do
        it "takes substring given start and end index" \_ -> do
          "snakes on a plane!" |> Text.slice 7 9 |> shouldBe "on"
          "snakes on a plane!" |> Text.slice 0 6 |> shouldBe "snakes"

        it "handles negative indices from the end" \_ -> do
          "snakes on a plane!" |> Text.slice 0 (-7) |> shouldBe "snakes on a"
          "snakes on a plane!" |> Text.slice (-6) (-1) |> shouldBe "plane"

        it "returns empty when end <= start" \_ -> do
          "snakes on a plane!" |> Text.slice 9 7 |> shouldBe ""

      describe "left" do
        it "takes n characters from the left" \_ -> do
          "Mulder" |> Text.left 2 |> shouldBe "Mu"

        it "handles n > length" \_ -> do
          "Mulder" |> Text.left 10 |> shouldBe "Mulder"

      describe "right" do
        it "takes n characters from the right" \_ -> do
          "Scully" |> Text.right 2 |> shouldBe "ly"

        it "handles n > length" \_ -> do
          "Scully" |> Text.right 10 |> shouldBe "Scully"

      describe "dropLeft" do
        it "drops n characters from the left" \_ -> do
          "The Lone Gunmen" |> Text.dropLeft 2 |> shouldBe "e Lone Gunmen"

      describe "dropRight" do
        it "drops n characters from the right" \_ -> do
          "Cigarette Smoking Man" |> Text.dropRight 2 |> shouldBe "Cigarette Smoking M"

    describe "Detect Substrings" do
      describe "contains" do
        it "checks if second string contains first" \_ -> do
          "theory" |> Text.contains "the" |> shouldBe True
          "theory" |> Text.contains "hat" |> shouldBe False
          "theory" |> Text.contains "THE" |> shouldBe False

      describe "startsWith" do
        it "checks if second string starts with first" \_ -> do
          "theory" |> Text.startsWith "the" |> shouldBe True
          "theory" |> Text.startsWith "ory" |> shouldBe False

      describe "endsWith" do
        it "checks if second string ends with first" \_ -> do
          "theory" |> Text.endsWith "the" |> shouldBe False
          "theory" |> Text.endsWith "ory" |> shouldBe True

      describe "indexes" do
        it "gets all indexes for a substring" \_ -> do
          "Mississippi" |> Text.indexes "i" |> shouldBe (Array.fromLinkedList [1, 4, 7, 10])
          "Mississippi" |> Text.indexes "ss" |> shouldBe (Array.fromLinkedList [2, 5])
          "haystack" |> Text.indexes "needle" |> shouldBe Array.empty

        it "returns empty array for empty needle" \_ -> do
          "haystack" |> Text.indexes "" |> shouldBe Array.empty

      describe "indices" do
        it "is an alias for indexes" \_ -> do
          "Mississippi" |> Text.indices "ss" |> shouldBe (Array.fromLinkedList [2, 5])

    describe "Formatting" do
      describe "toUpper" do
        it "converts text to upper case" \_ -> do
          "skinner" |> Text.toUpper |> shouldBe "SKINNER"

      describe "toLower" do
        it "converts text to lower case" \_ -> do
          "X-FILES" |> Text.toLower |> shouldBe "x-files"

      describe "pad" do
        it "pads text on both sides" \_ -> do
          "1" |> Text.pad 5 ' ' |> shouldBe "  1  "
          "11" |> Text.pad 5 ' ' |> shouldBe "  11 "
          "121" |> Text.pad 5 ' ' |> shouldBe " 121 "

      describe "padLeft" do
        it "pads text on the left" \_ -> do
          "1" |> Text.padLeft 5 '.' |> shouldBe "....1"
          "11" |> Text.padLeft 5 '.' |> shouldBe "...11"
          "121" |> Text.padLeft 5 '.' |> shouldBe "..121"

      describe "padRight" do
        it "pads text on the right" \_ -> do
          "1" |> Text.padRight 5 '.' |> shouldBe "1...."
          "11" |> Text.padRight 5 '.' |> shouldBe "11..."
          "121" |> Text.padRight 5 '.' |> shouldBe "121.."

      describe "trim" do
        it "gets rid of whitespace on both sides" \_ -> do
          "  hats  \n" |> Text.trim |> shouldBe "hats"

      describe "trimLeft" do
        it "gets rid of whitespace on the left" \_ -> do
          "  hats  \n" |> Text.trimLeft |> shouldBe "hats  \n"

      describe "trimRight" do
        it "gets rid of whitespace on the right" \_ -> do
          "  hats  \n" |> Text.trimRight |> shouldBe "  hats"

    describe "HTML Escaping" do
      describe "escapeHtml" do
        it "escapes HTML special characters" \_ -> do
          "<script>alert('XSS')</script>" |> Text.escapeHtml |> shouldBe "&lt;script&gt;alert(&#x27;XSS&#x27;)&lt;/script&gt;"

    describe "Conversions" do
      describe "toInt" do
        it "converts valid integer strings" \_ -> do
          "123" |> Text.toInt |> shouldBe (Just 123)
          "-42" |> Text.toInt |> shouldBe (Just (-42))
          "+42" |> Text.toInt |> shouldBe (Just 42)

        it "fails on improperly formatted strings" \_ -> do
          "3.1" |> Text.toInt |> shouldBe Nothing
          "31a" |> Text.toInt |> shouldBe Nothing

      describe "fromInt" do
        it "converts an Int to a Text" \_ -> do
          123 |> Text.fromInt |> shouldBe "123"
          (-42) |> Text.fromInt |> shouldBe "-42"

      describe "toFloat" do
        it "converts valid float strings" \_ -> do
          "123" |> Text.toFloat |> shouldBe (Just 123.0)
          "-42" |> Text.toFloat |> shouldBe (Just (-42.0))
          "3.1" |> Text.toFloat |> shouldBe (Just 3.1)
          ".5" |> Text.toFloat |> shouldBe (Just 0.5)

        it "fails on improperly formatted strings" \_ -> do
          "31a" |> Text.toFloat |> shouldBe Nothing

      describe "fromFloat" do
        it "converts a Float to a Text" \_ -> do
          123 |> Text.fromFloat |> shouldBe "123.0"
          (-42) |> Text.fromFloat |> shouldBe "-42.0"
          3.9 |> Text.fromFloat |> shouldBe "3.9"

      describe "toArray / fromArray" do
        it "converts between Text and Array Char" \_ -> do
          "abc" |> Text.toArray |> shouldBe (Array.fromLinkedList ['a', 'b', 'c'])
          "🙈🙉🙊" |> Text.toArray |> shouldBe (Array.fromLinkedList ['🙈', '🙉', '🙊'])
          ['a', 'b', 'c'] |> Array.fromLinkedList |> Text.fromArray |> shouldBe "abc"

      describe "fromChar / cons / uncons" do
        it "creates text from a char" \_ -> do
          'a' |> Text.fromChar |> shouldBe "a"

        it "adds a char to the beginning" \_ -> do
          "he truth is out there" |> Text.cons 'T' |> shouldBe "The truth is out there"

        it "splits non-empty text into head and tail" \_ -> do
          "abc" |> Text.uncons |> shouldBe (Just ('a', "bc"))

        it "returns Nothing for empty text uncons" \_ -> do
          "" |> Text.uncons |> shouldBe Nothing

    describe "Higher-Order Functions" do
      describe "map" do
        it "transforms every character" \_ -> do
          "a/b/c" |> Text.map (\c -> if c == '/' then '.' else c) |> shouldBe "a.b.c"

      describe "filter" do
        it "keeps characters that pass the test" \_ -> do
          "R2-D2" |> Text.filter (\c -> c >= '0' && c <= '9') |> shouldBe "22"

      describe "foldl" do
        it "reduces from the left" \_ -> do
          "time" |> Text.foldl Text.cons "" |> shouldBe "emit"

      describe "foldr" do
        it "reduces from the right" \_ -> do
          "time" |> Text.foldr Text.cons "" |> shouldBe "time"

      describe "any" do
        it "determines whether any characters pass the test" \_ -> do
          "90210" |> Text.any (\c -> c >= '0' && c <= '9') |> shouldBe True
          "R2-D2" |> Text.any (\c -> c >= '0' && c <= '9') |> shouldBe True
          "heart" |> Text.any (\c -> c >= '0' && c <= '9') |> shouldBe False

      describe "all" do
        it "determines whether all characters pass the test" \_ -> do
          "90210" |> Text.all (\c -> c >= '0' && c <= '9') |> shouldBe True
          "R2-D2" |> Text.all (\c -> c >= '0' && c <= '9') |> shouldBe False
          "heart" |> Text.all (\c -> c >= '0' && c <= '9') |> shouldBe False

    describe "Casing" do
      describe "conversions" do
        it "converts to PascalCase" \_ -> do
          "hello world" |> Text.toPascalCase |> shouldBe "HelloWorld"

        it "converts to camelCase" \_ -> do
          "Hello World" |> Text.toCamelCase |> shouldBe "helloWorld"

        it "converts to Title Case" \_ -> do
          "hello world" |> Text.toTitleCase |> shouldBe "Hello World"

        it "converts to snake_case" \_ -> do
          "Hello World" |> Text.toSnakeCase |> shouldBe "hello_world"

        it "converts to kebab-case" \_ -> do
          "Hello World" |> Text.toKebabCase |> shouldBe "hello-world"

      describe "checks" do
        it "checks isPascalCase" \_ -> do
          "HelloWorld" |> Text.isPascalCase |> shouldBe True
          "helloWorld" |> Text.isPascalCase |> shouldBe False

        it "checks isCamelCase" \_ -> do
          "helloWorld" |> Text.isCamelCase |> shouldBe True
          "HelloWorld" |> Text.isCamelCase |> shouldBe False

        it "checks isTitleCase" \_ -> do
          "Hello World" |> Text.isTitleCase |> shouldBe True
          "hello world" |> Text.isTitleCase |> shouldBe False

        it "checks isSnakeCase" \_ -> do
          "hello_world" |> Text.isSnakeCase |> shouldBe True
          "helloWorld" |> Text.isSnakeCase |> shouldBe False

        it "checks isKebabCase" \_ -> do
          "hello-world" |> Text.isKebabCase |> shouldBe True
          "helloWorld" |> Text.isKebabCase |> shouldBe False
