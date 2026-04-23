-- | Global storage for the ADR-0055 DispatchRegistry.
--
-- Populated once by 'Application.run' before any request handling begins.
-- Uses the same unsafePerformIO pattern as 'Config.Global', which is safe
-- because:
--
-- 1. The registry is set exactly once at startup, before any emit calls
-- 2. The registry is immutable once set
-- 3. No race conditions are possible in the startup sequence
module Service.Integration.GlobalRegistry
  ( get,
    setGlobal,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import IO qualified
import Service.Integration.DispatchRegistry (DispatchRegistry)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE globalRegistryRef #-}
globalRegistryRef :: IORef DispatchRegistry
globalRegistryRef = unsafePerformIO (newIORef DispatchRegistry.empty)


{-# NOINLINE get #-}
get :: DispatchRegistry
get = unsafePerformIO (readIORef globalRegistryRef)


setGlobal :: DispatchRegistry -> IO.IO ()
setGlobal registry = writeIORef globalRegistryRef registry
