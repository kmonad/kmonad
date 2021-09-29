{-# LANGUAGE CPP #-}
module KMonad.App.Main.OS

where

import KMonad.Prelude
import KMonad.Util.Ctx

{- NOTE: OS-specific IO-tweaks go here

This module is *tiny* and already super ugly. If it gets any bigger it's
probably a good idea to just to the standard: 'module per OS' solution. Just
thought it was weird to create 4 new modules to handle 1 line of linux-specific
code.

This is to strictly enforce our 'CPP-only-allowed-in-OS.hs' rule

-}

#ifdef linux_HOST_OS
import System.Posix.Signals (Handler(Ignore), installHandler, sigCHLD)
#endif

-- | This 'Ctx' exists to perform any OS-specific tweaks.
--
-- Currently we install a signal-handler for linux, and do nothing for the other
-- OS-es. This 'Ctx' is the very outermost context we run, so it will happen
-- before anything else, and after everything else.
withOS :: UIO m => Ctx r m ()

#ifdef linux_HOST_OS
withOS = mkCtx $ \f -> do
  -- Ignore SIGCHLD to avoid zombie processes.
  liftIO . void $ installHandler sigCHLD Ignore Nothing
  f ()
#endif

#ifdef darwin_HOST_OS
withOS = pure ()
#endif

#ifdef mingw32_HOST_OS
withOS = pure ()
#endif
