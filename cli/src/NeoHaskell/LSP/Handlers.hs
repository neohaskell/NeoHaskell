module NeoHaskell.LSP.Handlers
  ( handlers
  )
where

import Core

import Control.Concurrent.STM (atomically, modifyTVar', writeTVar)
import Control.Monad.IO.Class (liftIO)
import Map qualified
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS (virtualFileText)
import Console qualified

import NeoHaskell.LSP.State (ServerState (..))


-- | All LSP handlers for the NeoHaskell language server.
--
-- Combines notification handlers (lifecycle events) with request handlers
-- (formatting, semantic tokens) into a single 'LSP.Handlers' value.
handlers :: ServerState -> LSP.Handlers (LSP.LspM ())
handlers state = initializedHandler state
  ++ didOpenHandler state
  ++ didChangeHandler state
  ++ didSaveHandler state
  ++ didCloseHandler state
  ++ formattingHandler
  ++ semanticTokensHandler


-- ---------------------------------------------------------------------------
-- Notification handlers
-- ---------------------------------------------------------------------------

-- | Handle @initialized@ notification.
--
-- Reads the project root from the server environment and stores it
-- in 'ServerState'. Logs the root path to stderr for debugging.
initializedHandler :: ServerState -> LSP.Handlers (LSP.LspM ())
initializedHandler state =
  LSP.notificationHandler Msg.SMethod_Initialized \_ -> do
    env <- LSP.getLspEnv
    let root = LSP.resRootPath env
    liftIO do
      atomically (writeTVar state.projectRoot root)
      case root of
        Just path ->
          Console.error [fmt|[neohaskell-lsp] Initialized with root: #{path}|]
        Nothing ->
          Console.error "[neohaskell-lsp] Initialized (no root path)"


-- | Handle @textDocument/didOpen@ notification.
--
-- Stores the document content in the 'documents' TVar and publishes
-- empty diagnostics (clearing any stale errors for this URI).
didOpenHandler :: ServerState -> LSP.Handlers (LSP.LspM ())
didOpenHandler state =
  LSP.notificationHandler Msg.SMethod_TextDocumentDidOpen \msg -> do
    let uri = msg._params._textDocument._uri
        nuri = LSP.toNormalizedUri uri
        content = msg._params._textDocument._text
    liftIO (atomically (modifyTVar' state.documents (Map.set nuri content)))
    clearDiagnostics uri


-- | Handle @textDocument/didChange@ notification.
--
-- Uses full document sync ('TextDocumentSyncKind_Full'): the client sends
-- the entire file content on every change. We read the updated content from
-- the VFS (which the @lsp@ library updates before calling our handler).
didChangeHandler :: ServerState -> LSP.Handlers (LSP.LspM ())
didChangeHandler state =
  LSP.notificationHandler Msg.SMethod_TextDocumentDidChange \msg -> do
    let uri = msg._params._textDocument._uri
        nuri = LSP.toNormalizedUri uri
    mfile <- LSP.getVirtualFile nuri
    case mfile of
      Just vf -> do
        let content = virtualFileText vf
        liftIO (atomically (modifyTVar' state.documents (Map.set nuri content)))
      Nothing ->
        pure ()


-- | Handle @textDocument/didSave@ notification.
--
-- With @includeText = True@ in our sync options, the save notification
-- carries the full document text. We read from the VFS for consistency.
didSaveHandler :: ServerState -> LSP.Handlers (LSP.LspM ())
didSaveHandler state =
  LSP.notificationHandler Msg.SMethod_TextDocumentDidSave \msg -> do
    let uri = msg._params._textDocument._uri
        nuri = LSP.toNormalizedUri uri
    mfile <- LSP.getVirtualFile nuri
    case mfile of
      Just vf -> do
        let content = virtualFileText vf
        liftIO (atomically (modifyTVar' state.documents (Map.set nuri content)))
      Nothing ->
        pure ()


-- | Handle @textDocument/didClose@ notification.
--
-- Removes the document from our state and publishes empty diagnostics
-- to clear any errors from the Problems panel. Per LSP spec, stale
-- diagnostics persist until explicitly cleared.
didCloseHandler :: ServerState -> LSP.Handlers (LSP.LspM ())
didCloseHandler state =
  LSP.notificationHandler Msg.SMethod_TextDocumentDidClose \msg -> do
    let uri = msg._params._textDocument._uri
        nuri = LSP.toNormalizedUri uri
    liftIO (atomically (modifyTVar' state.documents (Map.remove nuri)))
    clearDiagnostics uri


-- ---------------------------------------------------------------------------
-- Request handlers (stubs)
-- ---------------------------------------------------------------------------

-- | Handle @textDocument/formatting@ request.
--
-- Returns empty edits (no formatting changes). The capability is declared
-- so the client knows the server supports it — actual formatting will be
-- implemented when the transpiler is integrated (Phase 2).
formattingHandler :: LSP.Handlers (LSP.LspM ())
formattingHandler =
  LSP.requestHandler Msg.SMethod_TextDocumentFormatting \_req responder -> do
    responder (Right (LSP.InL []))


-- | Handle @textDocument/semanticTokens/full@ request.
--
-- Returns null (no tokens). The capability is declared with a legend
-- containing standard token types — actual token computation will be
-- implemented when the transpiler is integrated (Phase 2).
semanticTokensHandler :: LSP.Handlers (LSP.LspM ())
semanticTokensHandler =
  LSP.requestHandler Msg.SMethod_TextDocumentSemanticTokensFull \_req responder -> do
    responder (Right (LSP.InR LSP.Null))


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Publish empty diagnostics for a URI, clearing any previously reported errors.
--
-- This is required by the LSP spec: if you published errors and the file
-- becomes valid, you MUST send @publishDiagnostics@ with an empty list.
-- The URI must exactly match what the client sent (no path normalization).
clearDiagnostics :: LSP.Uri -> LSP.LspM () ()
clearDiagnostics uri =
  LSP.sendNotification Msg.SMethod_TextDocumentPublishDiagnostics
    LSP.PublishDiagnosticsParams
      { _uri = uri
      , _version = Nothing
      , _diagnostics = []
      }
