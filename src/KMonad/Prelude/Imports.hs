{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module KMonad.Prelude.Imports
  ( module X )
where

import Control.Lens       as X
import Control.Monad.Cont as X
import Data.Acquire       as X
import GHC.Conc           as X (orElse)
import RIO.List           as X (nub, sort)
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
  )
