{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy.Rules (Rule (..), config) where

import Data.Text (Text)
import GHC.Generics (Generic)


data Rule = Rule
  { match :: Text,
    print :: Text
  }
  deriving (Generic)


-- Define your configuration as a list of rules directly in Haskell.
config :: [Rule]
config =
  [ Rule "(>>[ICS]>)|(<[ICS]<<)|(>[ICS]>)|(<[ICS]<)" "",
    Rule "Couldn't match type" "Couldn't match",
    Rule "               with" "          with",
    Rule "Expected type:" "Expected:",
    Rule "  Actual type:" "     Got:",
    Rule "Couldn't match expected type ‘(.*?)’" "Expected: $1",
    Rule "            with actual type ‘(.*?)’" "     Got: $1",
    Rule
      "Couldn't match expected type ‘(.*)’ with actual type ‘(.*)’"
      "Expected: $1\n     Got: $2",
    -- , Rule "(?s)>>C>.*?<C<<"                    ""
    Rule "(?s)In a stmt of a 'do' block:.*?<(C|S)<<" "<$1<<",
    Rule "(?s)In the \\w+ argument of.*?<(C|S)<<" "<$1<<",
    Rule "(?s)In the expression.*?<(C|S)<<" "<$1<<",
    Rule "\\(bound at ([^)]*)\\)" " -- $1",
    Rule
      "Ambiguous type variable (‘\\w+’) arising from a use of (‘\\w+’)"
      "Type variable $1 is ambiguous in $2.",
    Rule
      "prevents the constraint (‘.+’) from being solved.*"
      "Can't pick an instance for $1.",
    Rule
      "(Probable|Possible) fix:"
      "---\nMaybe-fix:",
    Rule
      "use a type annotation to specify what.*"
      "add type annotations to disambiguate.",
    Rule
      "No instance for (.*?) arising from (a|the)( use of)? (.*)"
      "Need a $1 instance for usage of $4",
    Rule
      "(?s)the type signature for:\n  "
      "\n",
    Rule
      "(?s)These potential instances .*? -fprint-potential-instances to see them all\\)"
      "More info: compile with -fprint-potential-instances.",
    Rule
      "Relevant bindings include"
      "Known types:\n"
  ]
