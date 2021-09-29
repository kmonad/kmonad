{-# LANGUAGE QuantifiedConstraints #-}
module KMonad.Model.IO

where

import KMonad.Prelude
import KMonad.Util
import KMonad.Model.Types

import UnliftIO.STM

withModel :: LUIO m e => ModelCfg -> Ctx r m ModelAPI
withModel cfg = mkCtx $ \f -> undefined

-- | Transmit 'KeyEvent's across the API
sendToModel, sendToShell :: (EnvIO m env, HasModelAPI env, HasKeyEvent e) => e -> m ()
sendToModel e = view toModel >>= \q -> atomically $ writeTQueue q (e^.keyEvent)
sendToShell e = view toShell >>= \q -> atomically $ writeTQueue q (e^.keyEvent)

-- | Receive 'KeyEvent's from across the API
recvFromModel, recvFromShell :: (EnvIO m env, HasModelAPI env) => m KeyEvent
recvFromModel = view toShell >>= \q -> atomically $ readTQueue q
recvFromShell = view toModel >>= \q -> atomically $ readTQueue q
