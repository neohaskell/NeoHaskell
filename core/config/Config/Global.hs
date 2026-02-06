-- | Internal module for global configuration storage.
--
-- This module provides type-safe global config access using 'Dynamic' for type erasure
-- and 'Typeable' for type recovery. The global IORef is safe because:
--
-- 1. Config is loaded FIRST before any services start
-- 2. Config is immutable once loaded
-- 3. No race conditions are possible in the startup sequence
--
-- This module is internal to nhcore. Users should access config via:
--
-- * Implicit parameter: @?config@ (with @HasXxxConfig@ constraint)
-- * Explicit accessor: @Config.get \@AppConfig@
module Config.Global (
  -- * Public API
  get,

  -- * Internal (for Application.run only)
  setGlobalConfig,
) where

import Appendable ((++))
import Basics
import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import IO qualified
import Maybe (Maybe (..))
import System.IO.Unsafe (unsafePerformIO)
import Text (Text)


-- | Global storage for application config.
--
-- Uses 'unsafePerformIO' which is safe because:
--
-- 1. Config is loaded FIRST before any services start
-- 2. Config is immutable once loaded
-- 3. No race conditions are possible in the startup sequence
{-# NOINLINE globalConfigRef #-}
globalConfigRef :: IORef (Maybe Dynamic)
globalConfigRef = unsafePerformIO (newIORef Nothing)


-- | Get the global config.
--
-- This function retrieves the application config that was loaded during
-- 'Application.run'. It uses 'Typeable' to recover the concrete type
-- from the type-erased 'Dynamic' storage.
--
-- __IMPORTANT__: This function MUST be called after 'Application.run' has
-- loaded the config. Calling it before will result in a runtime error.
--
-- @
-- myFunction :: Task err ()
-- myFunction = do
--   let port = (Config.get \@AppConfig).port
--   Console.print [fmt|Using port: {port}|]
-- @
{-# NOINLINE get #-}
get :: forall config. (Typeable config, HasCallStack) => config
get = unsafePerformIO do
  maybeDyn <- readIORef globalConfigRef
  case maybeDyn of
    Nothing ->
      panic ("Config.get: Config not initialized. Ensure Application.run has been called with withConfig." ++ configDependentGuidance)
    Just dyn ->
      case fromDynamic dyn of
        Just config -> pure config
        Nothing -> do
          let expectedType = Typeable.typeRep (Typeable.Proxy @config)
          let actualType = dynTypeRep dyn
          panic
            [fmt|Config.get: Type mismatch. Expected #{expectedType} but config has type #{actualType}.|]


-- | Guidance text for Config.get errors.
--
-- This constant is used in error messages to provide consistent guidance
-- about the config-dependent API pattern. Update this single location
-- when adding new @*From@ helper functions.
configDependentGuidance :: Text
configDependentGuidance =
  "\n\nIf you need config values to build your Application, use:\n\
  \  |> Application.withEventStoreFrom @YourConfig makeEventStoreConfig\n\
  \  |> Application.withFileUploadFrom @YourConfig makeFileUploadConfig"


-- | Internal: Set the global config.
--
-- This is called by 'Application.run' after loading the config.
-- It should NOT be called directly by user code.
setGlobalConfig :: (Typeable config) => config -> IO.IO ()
setGlobalConfig config = writeIORef globalConfigRef (Just (toDyn config))
