module Neo.Build.Templates.CabalProject (
  template,
) where

import Neo.Core


template :: Text
template = do
  [fmt|packages:
    .

source-repository-package
  type: git
  location: https://github.com/NeoHaskell/NeoHaskell.git
  tag: 42a5f752eb3135246abb24484e9cc8570b1626d3
  subdir: core
  --sha256: 1pgkylxbd647ay6wb9llck09xbqj8k9bf6sjds7fics3ckxff66s

|]
