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
  tag: main
  subdir: core

|]
