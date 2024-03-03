let Rule : Type = { match : Text, print : Text }
let Config : Type = { rules : List Rule }

let rule = \(match: Text) -> \(replace: Text) ->
  { match = match, print = replace } : Rule

in {
  rules =
    [
      rule "(>>[ICS]>)|(<[ICS]<<)|(>[ICS]>)|(<[ICS]<)" ""
    , rule "Couldn't match type"                  "Couldn't match"
    , rule "               with"                  "          with"
    , rule "Expected type:"                       "Expected:"
    , rule "  Actual type:"                       "     Got:"
    , rule "Couldn't match expected type ‘(.*?)’" "Expected: $1"
    , rule "            with actual type ‘(.*?)’" "     Got: $1"
    , rule "Couldn't match expected type ‘(.*)’ with actual type ‘(.*)’"
           ''
           Expected: $1
                Got: $2
           ''
    -- , rule "(?s)>>C>.*?<C<<"                    ""
    , rule "(?s)In a stmt of a 'do' block:.*?<(C|S)<<" "<$1<<"
    , rule "(?s)In the \\w+ argument of.*?<(C|S)<<" "<$1<<"
    , rule "(?s)In the expression.*?<(C|S)<<"       "<$1<<"
    , rule "\\(bound at ([^)]*)\\)"             " -- $1"
    , rule
        "Ambiguous type variable (‘\\w+’) arising from a use of (‘\\w+’)"
        "Type variable $1 is ambiguous in $2."
    , rule
        "prevents the constraint (‘.+’) from being solved.*"
        "Can't pick an instance for $1."
    , rule
        "(Probable|Possible) fix:"
        ''
        ---
        Maybe-fix:''
    , rule
        "use a type annotation to specify what.*"
        "add type annotations to disambiguate."
    , rule
        "No instance for (.*?) arising from (a|the)( use of)? (.*)"
        "Need a $1 instance for usage of $4"
    , rule
        "(?s)the type signature for:\n  "
        "\n"
    , rule
        "(?s)These potential instances .*? -fprint-potential-instances to see them all\\)"
        "More info: compile with -fprint-potential-instances."
    , rule
        "Relevant bindings include"
        "Known types:\n"
    ] : List Rule
} : Config
