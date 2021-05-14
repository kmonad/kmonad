{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-|
Module      : KMonad.Prelude
Description : Code that will be imported into every module
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}

module KMonad.Prelude
  ( module X
  , MIO, UIO, EnvIO, EnvUIO
  )
where

import Control.Lens       as X
import Control.Monad.Cont as X
import Data.Acquire       as X
import GHC.Conc           as X (orElse)
import RIO.Text           as X (unlines, lines, unpack, pack)

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines

    -- Will import these when I need it
  , some, many

    -- Names I'd like to use myself
  , wait
  )

--------------------------------------------------------------------------------
-- $shorthand

-- | Shorthand for a `MonadIO` constraint, I'd prefer using `IO`, but rebinding
-- actual IO to free up that name seems needlessly confusing
type MIO m = MonadIO m

-- | Shorthand for MonadUnliftIO
type UIO m = MonadUnliftIO m

-- | Shorthand for MonadIO and MonadReader
type EnvIO m e = (MIO m, MonadReader e m)

type EnvUIO m e = (UIO m, MonadReader e m)
