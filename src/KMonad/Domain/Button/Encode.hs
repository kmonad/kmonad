module KMonad.Domain.Button.Encode
  ( encode
  )
where

import Control.Monad.Trans

import KMonad.Core.Button
import KMonad.Core.Parser
import KMonad.Domain.Button.Buttons
import KMonad.Domain.Effect (CanButton)


-- | Turn a ButtonToken into a Button operation
encode :: (CanButton m, MonadIO n) => KeyCode -> ButtonToken -> n (Button m)
encode _ (BAfter a b) = mkAfter <$> encode a <*> encode b
encode _ (BEmit kc)     = mkEmitM kc
encode _ (BEmitSpecial ss) = mkEmitSpecialM ss
encode _ (BEmitDeadKey dk) = mkEmitDeadKeyM dk
encode _ (BModded kc b) = do
  x <- mkEmitM kc
  y <- encode  b
  mkAroundM x y
encode _ BBlock = mkBlockM
encode _ (BLayerToggle lid) = mkLayerToggleM kc lid
encode _ (BLayerAdd lid) = mkLayerAddM lid
encode _ (BLayerRem lid) = mkLayerRemM lid
encode c (BTapHold ms bt bh) = do
  btap <- encode bt
  bhld <- encode bh
  mkTapHold c ms btap bhld
encode c (BTapNext bt bh) = do
  btap <- encode bt
  bhld <- encode bh
  mkTapNext c btap bhld
encode _ (BMacro bs) = mkMacroM bs
encode _ (BMultiTap bs) = mkMultiTapM =<< mapM (\(t, b) -> (t,) <$> encode b) bs
encode _ (BLockOn lk) = mkLockOnM lk
encode _ (BLockOff lk) = mkLockOffM lk
encode _ (BLockToggle lk) = mkLockToggleM lk
