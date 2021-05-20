{-|
Module      : KMonad.Pullchain.Components.Keymap
Description : Implementation of mapping key-presses to button actions
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

In KMonad we handle all releases (and some other actions) via callback
mechanisms. It is the button-presses that get handled through a keymap. It is
the 'Keymap' component that manages the keymap state and ensures that
incoming events are mapped to

-}
module KMonad.Pullchain.Components.Keymap
  ( Keymap
  , mkKeymap
  , layerOp
  , lookupKey
  )
where


import KMonad.Prelude
import KMonad.Util
import KMonad.Util.Keyboard
import KMonad.Util.Logging

import KMonad.Pullchain.Action
import KMonad.Pullchain.Button
import KMonad.Pullchain.Types hiding (layerOp)

import qualified KMonad.Util.LayerStack as Ls

--------------------------------------------------------------------------------
-- $env
--

-- | The 'Keymap' environment containing the current keymap
--
-- NOTE: Since the 'Keymap' will never have to deal with anything
-- asynchronously we can simply use 'IORef's here.
data Keymap = Keymap
  { _stack :: IORef (LMap BEnv)
  , _baseL :: IORef Name
  }
makeClassy ''Keymap

-- | Create a 'Keymap' from a 'Keymap' of uninitialized 'Button's and a
-- tag indicating which layer should start as the base.
mkKeymap' :: MonadUnliftIO m
  => Name    -- ^ The initial base layer
  -> LMap Button -- ^ The keymap of 'Button's
  -> m Keymap
mkKeymap' n m = do
  envs <- m & Ls.items . itraversed %%@~ \(_, c) b -> mkBEnv b c
  Keymap <$> newIORef envs <*> newIORef n

-- | Create a 'Keymap' but do so in the context of a 'Ctx' monad to ease nesting.
mkKeymap :: MonadUnliftIO m => Name -> LMap Button -> Ctx r m Keymap
mkKeymap n = lift . mkKeymap' n


--------------------------------------------------------------------------------
-- $op
--
-- The following code describes how we add and remove layers from the
-- 'Keymap'.

-- | Print a header message followed by an enumeration of the layer-stack
debugReport :: LIO m e => Keymap -> Text -> m ()
debugReport h hdr = do
  st <- view Ls.stack <$> (readIORef $ h^.stack)
  let ub = foldMap (\(i, n) -> " "  <> tshow i
                            <> ". " <> tshow n <> "\n")
             (zip ([1..] :: [Int]) st)
  ls <- readIORef (h^.baseL)
  logDebug $ hdr <> "\n" <> ub <> "Base-layer: " <> ls <> "\n"

-- | Perform operations on the layer-stack
layerOp :: LIO m e
  => Keymap  -- ^ The 'Keymap' environment
  -> LayerOp -- ^ The 'LayerOp' to perform
  -> m ()    -- ^ The resulting action
layerOp h o = let km = h^.stack in case o of
  (PushLayer n) -> do
    Ls.pushLayer n <$> readIORef km >>= \case
      Left e   -> throwIO e
      Right m' -> writeIORef km m'
    debugReport h $ "Pushed layer to stack: " <> tshow n

  (PopLayer n) -> do
    Ls.popLayer n <$> readIORef km >>= \case
      Left (Ls.LayerNotOnStack _) -> do
        debugReport h $ "WARNING: Tried popping layer that was not on stack " <> tshow n
      Left e                      -> throwIO e
      Right m'                    -> do
        writeIORef km m'
        debugReport h $ "Popped layer from stack: " <> tshow n

  (SetBaseLayer n) -> do
    (n `elem`) . view Ls.maps <$> (readIORef km) >>= \case
      True  -> writeIORef (h^.baseL) n
      False -> throwIO $ Ls.LayerDoesNotExist n
    debugReport h $ "Set base layer to: " <> tshow n


--------------------------------------------------------------------------------
-- $run
--
-- How we use the 'Keymap' to handle events.

-- | Lookup the 'BEnv' currently mapped to the key press.
lookupKey :: MonadIO m
  => Keymap   -- ^ The 'Keymap' to lookup in
  -> Keycode        -- ^ The 'Keycode' to lookup
  -> m (Maybe BEnv) -- ^ The resulting action
lookupKey h c = do
  m <- readIORef $ h^.stack
  f <- readIORef $ h^.baseL

  pure $ case m ^? Ls.atKey c of
    Nothing -> m ^? Ls.inLayer f c
    benv    -> benv
