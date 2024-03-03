# NeoHaskell GHC Clippy Plugin

A helpful companion to GHC.

Overrides GHC error and warning messages, to the user's liking.

Configured using (how else!) regexps. Tested with stack and ghcid.

## Showcase

<img align="right" width="409" src="https://i.imgur.com/Beay7p4.png">
<img width="409" src="https://i.imgur.com/CsYulBQ.png">

Left: without Clippy.

Right: with Clippy, using the sample config.

## But, why?

For all kinds of reasons:
 - making GHC messages more terse or more verbose
 - adding more context for beginners
 - stripping confusing / duplicated / rarely useful context for everyone
 - prototyping improvements for ghc error output
 - ever wanted GHC to speak in emoji? :smiling_imp:
 - ... or in your mother tongue?
 - ... or in mathematical notation?

## Usage

1. Add the `ghc-clippy-plugin` dependency to your project
2. Pass `-fplugin=Clippy` in GHC options

E.g. for stack:

```yaml
# file: package.yaml
dependencies:
 - ghc-clippy-plugin

ghc-options:
- -fplugin=Clippy
```

3. When building, you should see the following warning:

```
ghc-clippy-plugin: warning:
    Clippy plugin couldn't start. Cause:
    ./.clippy.dhall: openFile: does not exist (No such file or directory)
```

Make sure there was anything to compile (change a .hs file) if the warning wasn't there.

Save the [sample config](/.clippy-terse.dhall) as `.clippy.dhall` in the project's root dir
(or - more precisely - the `current directory` GHC is going to use)

4. Put an error somewhere:

```haskell
oops = print mempty
```

With the sample config, this should output:

```
./app/Main.hs:19:14-19: error:
    Type variable ‘a0’ is ambiguous in ‘mempty’.
    Can't pick an instance for ‘(Monoid a0)’.
    ---
    Maybe-fix: add type annotations to disambiguate.
    More info: compile with -fprint-potential-instances.
   |
19 | oops = print mempty
   |
```

5. Enjoy the much terser output and tweak it to your heart's content! :grin:


## Tweaking the config

### Stack

For `--file-watch` to pick up config changes, add in `package.yaml`:
```yaml
extra-source-files:
- .clippy.dhall
```

Use `stack build --file-watch`. Make sure to have some errors handy! :)

### Ghcid

For ghcid to reload after config change, run it with `--reload=.clippy.dhall`.
I tend to put the above line in `./.ghcid`.

Ghcid may terminate if there are compile errors on startup.
If that's the case, remove your errors until ghcid starts successfully :)

With the above, for error messages, ghcid picks up `.clippy.dhall` changes immediately.

For warnings, one needs to trigger recompilation of the file triggering them.
One way around that is to enable `-Werror` for the period of config tweaking.
(I put mine in `./.ghci`.)

### Error message structure, section markers

In GHC, each error/warning message contains 3 sections in its [ErrDoc](https://hackage.haskell.org/package/ghc-8.10.1/docs/src/ErrUtils.html#ErrDoc)
('Important', 'Context', and 'Supplementary'). Each section contains a list of `MsgDoc`-s.

Before replacing the message text, Clippy wraps each section, and each of their `MsgDoc`-s, with
markers. This allows for more precise match targeting, including MsgDoc/section ends.

To see the structure of the replaced error message, comment out the `marker removing rule` in the
config.

```dhall
  rules =
    [ -- rule "(>>[ICS]>)|(<[ICS]<<)|(>[ICS]>)|(<[ICS]<)" ""
    , rule "..." "..."
    -- ...
    ]
```

Comment out all rules to see the structure of the original message. Sample result:

```
./app/Main.hs:27:11: error:
    • >>I>
      >I>
      No instance for (Num a) arising from a use of ‘+’
      Possible fix:
        add (Num a) to the context of
          the type signature for:
            bar :: forall a. a -> a -> a
      <I<
      <I<<
    • >>C>
      >C>
      In the expression: a + b
      In an equation for ‘bar’: bar a b = a + b
      <C<
      <C<<
    • >>S>
      <S<<
   |
27 | bar a b = a + b
   |
```

Replace rules can span across multiple messages in a section, but can't cross section boundaries.
For example, the following rule will remove the entire 'Context' section:

```dhall
  rule "(?s)>>C>.*?<C<<" "" -- notice the (?s) - "dot-all" regex flag
```

All-whitespace lines are removed from all the messages.

### Rule matching order

Replacement rules are applied in reverse order of the `rules` list in config.
This means the most generic and least selective rules should go at the top of the file.
In particular, the `marker removing rule` - which should be applied last - should be
the first rule in every config.


## Acknowledgements

I'd like to thank the authors of
 - the [Scala Clippy Plugin](https://scala-clippy.org/) for the name and the idea
 - [Error Messages in Haskell, and how to Improve
   them](https://anthony.noided.media/blog/haskell/programming/2020/05/14/haskell-errors.html) for
   inspiration and some initial test cases
 - [Understanding Basic Haskell Error
Messages](http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_understanding_basic_haskell_error_messages.pdf)
   for test cases as well
 - the wonderful folk working on GHC, for making this possible :heart:

## Roadmap

- resolve the config from the first directory above that contains `.clippy.dhall`
- fall back to `~/.config/clippy.dhall` and then to `~/.clippy.dhall`
- cache the config instead of re-parsing for every module
- ...?

## License

The NeoHaskell GHC Clippy Plugin is a fork of [`ghc-clippy-plugin`](https://github.com/ArturGajowy/ghc-clippy-plugin). Which is licensed under the BSD3 License. For more information, check the [`LICENSE`](LICENSE) file in this folder.