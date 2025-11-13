module Neo.New.Templates.GitIgnore where

import Core


template :: Text
template = do
  [fmt|\# NeoHaskell build outputs
.neo/
dist/
build/

\# IDE
.idea/
*.swp
*.swo

\# OS
.DS_Store
Thumbs.db|]
