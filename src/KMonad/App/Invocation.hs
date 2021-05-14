{-

This module is responsible for parsing how KMonad is invoked from the
command-line into a 'Invoc' record.

-}
module KMonad.App.Invocation
  ( getInvoc )
where

import KMonad.App.Invocation.IO (getInvoc)
