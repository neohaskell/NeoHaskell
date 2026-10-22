# Example compiled at the consumer pin

This runnable example imports NeoHaskell's `Core` package.

```haskell top
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Core (Text)
```

```haskell
example :: Text
example = "exact SHA"

main :: IO ()
main = pure ()
```

This fragment explains a shape and is not a complete declaration.

```haskell ignore
example = ...
```
