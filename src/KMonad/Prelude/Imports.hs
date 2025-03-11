{-# LANGUAGE NoImplicitPrelude #-}

module KMonad.Prelude.Imports
  ( module X )
where

import GHC.Generics       as X (Generically(..))
import Control.Lens       as X
import Control.Monad.Cont as X
import Control.Monad.Except as X
import Data.Acquire       as X
import RIO.Char           as X
import RIO.Text           as X (unlines, lines, unpack, pack)
import Data.Foldable1     as X
import RIO.NonEmpty       as X (tail, nonEmpty, groupAllWith, some1)

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines

    -- Will import these when I need it
  , some, many
  , seq
  , try, timeout

    -- Use lenses instead
  , reverse -- `view reversed` or `reversing`

    -- Breaks Paths_kmonad
  , catchIO
  )
