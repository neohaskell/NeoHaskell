module NeoHaskell.LSP.Server
  ( -- * Running the server
    run
  )
where

import Prelude

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP

import NeoHaskell.LSP.Handlers (handlers)
import NeoHaskell.LSP.State (initState)


-- | Run the NeoHaskell LSP server over stdio.
--
-- Creates initial server state, then enters the @lsp@ library's main loop.
-- Returns an exit code (0 = success).
--
-- The server advertises:
--
-- * Full document sync (client sends entire file on every change)
-- * Document formatting (stub — returns empty edits)
-- * Semantic tokens (stub — returns null)
--
-- Position encoding defaults to UTF-16 (LSP standard). UTF-32 negotiation
-- will be added in a future phase.
run :: IO Int
run = do
  state <- initState
  LSP.runServer LSP.ServerDefinition
    { LSP.defaultConfig = ()
    , LSP.configSection = "neohaskell"
    , LSP.parseConfig = \_ _ -> Right ()
    , LSP.onConfigChange = \_ -> pure ()
    , LSP.doInitialize = \env _req -> pure (Right env)
    , LSP.staticHandlers = \_ -> handlers state
    , LSP.interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO
    , LSP.options = serverOptions
    }


-- | Server options defining LSP capabilities.
--
-- Sets full document sync with open\/close\/save notifications and
-- @includeText@ on save. Other capabilities (formatting, semantic tokens)
-- are inferred by the @lsp@ library from registered handlers.
serverOptions :: LSP.Options
serverOptions = LSP.defaultOptions
  { LSP.optTextDocumentSync = Just LSP.TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just LSP.TextDocumentSyncKind_Full
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Just (LSP.InR LSP.SaveOptions { _includeText = Just True })
      }
  }
