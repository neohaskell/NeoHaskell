module NeoHaskell.LSP.State
  ( -- * Types
    ServerState (..)
  , TranspileResult (..)
    -- * Construction
  , initState
  )
where

import Core

import Control.Concurrent.STM (TVar, newTVarIO)
import IO (FilePath)
import Map qualified
import Language.LSP.Protocol.Types qualified as LSP


-- | Placeholder for future transpilation results.
-- Will hold AST, errors, and other transpiler output in Phase 2A.
data TranspileResult = TranspileResult
  deriving (Show, Eq)


-- | Shared mutable state for the LSP server.
--
-- All fields use 'TVar' for thread-safe access. Currently single-threaded
-- (the @lsp@ library runs handlers on one thread), but this prepares for
-- Phase 2A where a background thread runs the transpiler on a debounce timer.
data ServerState = ServerState
  { documents :: TVar (Map LSP.NormalizedUri Text)
  -- ^ Currently open document contents, keyed by normalized URI.
  , lastGoodTranspilation :: TVar (Map LSP.NormalizedUri TranspileResult)
  -- ^ Last successful transpilation result per document.
  -- Used to provide stale-but-valid results while re-transpiling.
  , projectRoot :: TVar (Maybe FilePath)
  -- ^ Project root path, set during initialization from the client's @rootUri@.
  }


-- | Create a fresh 'ServerState' with empty maps and no project root.
initState :: IO ServerState
initState = do
  docs <- newTVarIO Map.empty
  transpiled <- newTVarIO Map.empty
  root <- newTVarIO Nothing
  pure ServerState
    { documents = docs
    , lastGoodTranspilation = transpiled
    , projectRoot = root
    }
