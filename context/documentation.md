# How to write documentation comments

All documentation comments (doc-comments) should be clear and concise,
they shouldn't use Haskell-specific terminology, and instead, they should
rely on other concepts of the current codebase (the NeoHaskell codebase).

The comments should be written in a way that is familiar to a junior TypeScript developer
TypeScript developer, using concepts from TypeScript as an anchor to
explain things better.

If the code is a function, it should include a doctest, which looks like
this:

```hs
-- >>> fib 10
-- 55
```

Even better, if the function has a property that can be established and
verified, the doctest will define a property based doctest like so:

```hs
-- prop> \(Small n) -> fib n == fib (n + 2) - fib (n + 1)
```

Comments will use Markdown format instead of Haddock format.
