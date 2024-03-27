{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy.Rules (Rule (..), rules) where

import Data.Text (Text)
import GHC.Generics (Generic)


data Rule = Rule
  { match :: Text,
    print :: Text
  }
  deriving (Generic)


-- These rules are applied in reverse order
rules :: [Rule]
rules =
  [ Rule "(>>[EW]>)|(<[EW]<<)|(>[EW]>)|(<[EW]<)" "",
    -- Rule "Couldn't match type" "Couldn't match",
    -- Rule "               with" "          with",
    -- Rule "Expected type:" "Expected:",
    -- Rule "  Actual type:" "     Got:",
    -- Rule "Couldn't match expected type ‘(.*?)’" "Expected: $1",
    -- Rule "            with actual type ‘(.*?)’" "     Got: $1",
    -- Rule
    --   "Couldn't match expected type ‘(.*)’ with actual type ‘(.*)’"
    --   "Expected: $1\n     Got: $2",
    -- Rule "(?s)>>E>.*?<E<<" "",
    -- Rule "(?s)In a stmt of a 'do' block:.*?<(E)<<" "<$1<<",
    -- Rule "(?s)In the \\w+ argument of.*?<(E)<<" "<$1<<",
    -- Rule "(?s)In the expression.*?<(E)<<" "<$1<<",
    -- Rule "\\(bound at ([^)]*)\\)" " -- $1",
    -- Rule
    --   "Ambiguous type variable (‘\\w+’) arising from a use of (‘\\w+’)"
    --   "Type variable $1 is ambiguous in $2.",
    -- Rule
    --   "prevents the constraint (‘.+’) from being solved.*"
    --   "Can't pick an instance for $1.",
    -- Rule
    --   "(Probable|Possible) fix:"
    --   "---\nMaybe-fix:",
    -- Rule
    --   "use a type annotation to specify what.*"
    --   "add type annotations to disambiguate.",
    -- Rule
    --   "No instance for (.*?) arising from (a|the)( use of)? (.*)"
    --   "Need a $1 instance for usage of $4",
    Rule
      "No instance for \\(([A-Z].*) ([A-Z].*)\\) arising from (a|the)( use of)? (.*)"
      "I'm trying to use $5, but in order to do that, the type\n\n\t$2\n\nshould implement the trait\n\n\t$1\n\nBut I don't see any implementation in scope.",
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
