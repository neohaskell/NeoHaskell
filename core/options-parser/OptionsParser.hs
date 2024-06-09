{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module OptionsParser (
  OptionsParser,
  FieldInfo,
) where

import Core
import Data.Record.Anon
import Data.Record.Anon.Advanced qualified as Advanced
import Data.Record.Anon.Simple (Record)
import Data.Record.Anon.Simple qualified as Simple
import Default (defaultValue)
import GHC.OverloadedLabels (IsLabel (..))
import Options.Applicative qualified as OptParse
import Text qualified


newtype OptionsParser value = OptionsParser (OptParse.Parser value)


type Config value =
  [ "long" := Text,
    "short" := Char,
    "metavar" := Text,
    "help" := Text,
    "showDefault" := Bool,
    "value" := value
  ]


newtype NamedFieldToMod value = NamedFieldToMod (Text -> value -> OptParse.Mod OptParse.OptionFields value)


newtype FieldInfo fieldType value = FieldInfo (OptParse.Mod fieldType value)


type OptionConverter value =
  [ "long" := (Text -> OptParse.Mod OptParse.OptionFields value),
    "short" := (Char -> OptParse.Mod OptParse.OptionFields value),
    "metavar" := (Text -> OptParse.Mod OptParse.OptionFields value),
    "help" := (Text -> OptParse.Mod OptParse.OptionFields value),
    "showDefault" := (Bool -> OptParse.Mod OptParse.OptionFields value),
    "value" := (value -> OptParse.Mod OptParse.OptionFields value)
  ]


optionConverter =
  ANON
    { long = \(longName :: Text) -> OptParse.long (Text.toLinkedList longName),
      short = \(shortName :: Char) -> OptParse.short shortName,
      metavar = \(metavarName :: Text) -> OptParse.metavar (Text.toLinkedList metavarName),
      help = \(helpText :: Text) -> OptParse.help (Text.toLinkedList helpText),
      showDefault = \(showDefault :: Bool) -> if showDefault then OptParse.showDefault else defaultValue,
      value = \value -> OptParse.value value
    }


x =
  ANON
    { long = "hello" :: Text,
      metavar = "TARGET" :: Text,
      help = "Target for the greeting" :: Text
    }


y = Advanced.ap optionConverter x

-- foo :: (KnownFields fields) => Advanced.Record NamedFieldToMod fields -> OptParse.Mod OptParse.OptionFields a
-- foo = fold . map toModField . fromList . recordToList

-- toModField :: (IsLabel fieldName fieldType) => (fieldName, fieldType) -> OptParse.Mod OptParse.OptionFields a
-- toModField ("long", longName :: Text) = OptParse.long (unpack longName)
-- toModField ("short", shortName :: Char) = OptParse.short shortName
-- toModField ("metavar", metavarName :: Text) = OptParse.metavar (unpack metavarName)
-- toModField ("help", helpText :: Text) = OptParse.help (unpack helpText)
-- toModField ("showDefault", showDefault :: Bool) = OptParse.showDefault showDefault
-- toModField ("value", value) = OptParse.value value
-- toModField _ = mempty
