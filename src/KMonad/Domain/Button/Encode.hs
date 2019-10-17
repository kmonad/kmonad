module KMonad.Domain.Button.Encode
  ( encode
  )
where

import Control.Monad.Trans

import KMonad.Core.Parser
import KMonad.Domain.Button.Button
import KMonad.Domain.Button.Buttons
import KMonad.Domain.Effect (CanButton)


-- | Turn a ButtonToken into a Button operation
encode :: (CanButton m, MonadIO io) => KeyCode -> ButtonToken -> io (Button m)
encode c (BAfter a b) = do
  ba <- encode c a
  bb <- encode c b
  mkAfter ba bb
encode _ (BEmit kc) = mkEmit kc
encode _ (BEmitSpecial ss) = mkEmitSpecial ss
encode _ (BEmitDeadKey dk) = mkEmitDeadKey dk
encode c (BModded kc b) = do
  x <- mkEmit kc
  y <- encode c b
  mkAround x y
encode _ BBlock = mkBlock
encode _ (BLayerToggle lid) = mkLayerToggle lid
encode _ (BLayerAdd lid) = mkLayerAdd lid
encode _ (BLayerRem lid) = mkLayerRem lid
encode c (BTapHold ms bt bh) = do
  btap <- encode c bt
  bhld <- encode c bh
  mkTapHold c ms btap bhld
encode c (BTapNext bt bh) = do
  btap <- encode c bt
  bhld <- encode c bh
  mkTapNext c btap bhld
encode _ (BMacro bs) = mkMacro bs
encode c (BMultiTap bs) = mkMultiTap c =<< mapM (\(t, b) -> (t,) <$> encode c b) bs
encode _ (BLockOn lk) = mkLockOn lk
encode _ (BLockOff lk) = mkLockOff lk
encode _ (BLockToggle lk) = mkLockToggle lk
