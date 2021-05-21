{-|
Module      : KMonad.Pullchain.Components.MapStack
Description : Implementation of mapping key-presses to button actions
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

In KMonad we handle all releases (and some other actions) via callback
mechanisms. It is the button-presses that get handled through a keymap. It is
the 'MapStack' component that manages the keymap state and ensures that
incoming events are mapped to

-}
module KMonad.Pullchain.Components.Keymap
  ( MapStack
  , mkKeymap
  , layerOp
  , lookupKey
  )
where


import KMonad.Prelude

import KMonad.Model
import KMonad.Util

import KMonad.Pullchain.Action
import KMonad.Pullchain.Button
import KMonad.Pullchain.Types hiding (layerOp)

import qualified KMonad.Util.LayerStack as L
import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $type

type Stack = L.LayerStack LayerName Keycode BEnv

--------------------------------------------------------------------------------
-- $env
--

-- | The 'MapStack' environment containing the current keymap
--
-- NOTE: Since the 'MapStack' will never have to deal with anything
-- asynchronously we can simply use 'IORef's here.
data MapStack = MapStack
  { _stack :: IORef Stack
  , _baseL :: IORef Name
  }
makeClassy ''MapStack

-- | Create a 'MapStack' from a 'Keymap' of uninitialized 'Button's and a
-- tag indicating which layer should start as the base.
mkKeymap' :: MonadUnliftIO m
  => Name    -- ^ The initial base layer
  -> Keymap Button
  -> m MapStack
mkKeymap' n km = do
  let m = L.mkLayerStack $ map (second M.toList) $ km
  envs <- m & L.items . itraversed %%@~ \(_, c) b -> mkBEnv b c
  MapStack <$> newIORef envs <*> newIORef n

-- | Create a 'MapStack' but do so in the context of a 'Ctx' monad to ease nesting.
mkKeymap :: MonadUnliftIO m
  => Name
  -> Keymap Button
  -> Ctx r m MapStack
mkKeymap n = lift . mkKeymap' n


--------------------------------------------------------------------------------
-- $op
--
-- The following code describes how we add and remove layers from the
-- 'MapStack'.

-- | Print a header message followed by an enumeration of the layer-stack
debugReport :: LIO m e => MapStack -> Text -> m ()
debugReport h hdr = do
  st <- view L.stack <$> (readIORef $ h^.stack)
  let ub = foldMap (\(i, n) -> " "  <> tshow i
                            <> ". " <> tshow n <> "\n")
             (zip ([1..] :: [Int]) st)
  ls <- readIORef (h^.baseL)
  logDebug $ hdr <> "\n" <> ub <> "Base-layer: " <> ls <> "\n"

-- | Perform operations on the layer-stack
layerOp :: LIO m e
  => MapStack  -- ^ The 'MapStack' environment
  -> LayerOp -- ^ The 'LayerOp' to perform
  -> m ()    -- ^ The resulting action
layerOp h o = let km = h^.stack in case o of
  (PushLayer n) -> do
    L.pushLayer n <$> readIORef km >>= \case
      Left e   -> throwIO e
      Right m' -> writeIORef km m'
    debugReport h $ "Pushed layer to stack: " <> tshow n

  (PopLayer n) -> do
    L.popLayer n <$> readIORef km >>= \case
      Left (L.LayerNotOnStack _) -> do
        debugReport h $ "WARNING: Tried popping layer that was not on stack " <> tshow n
      Left e                      -> throwIO e
      Right m'                    -> do
        writeIORef km m'
        debugReport h $ "Popped layer from stack: " <> tshow n

  (SetBaseLayer n) -> do
    (n `elem`) . view L.maps <$> (readIORef km) >>= \case
      True  -> writeIORef (h^.baseL) n
      False -> throwIO $ L.LayerDoesNotExist n
    debugReport h $ "Set base layer to: " <> tshow n


--------------------------------------------------------------------------------
-- $run
--
-- How we use the 'MapStack' to handle events.

-- | Lookup the 'BEnv' currently mapped to the key press.
lookupKey :: MonadIO m
  => MapStack   -- ^ The 'MapStack' to lookup in
  -> Keycode        -- ^ The 'Keycode' to lookup
  -> m (Maybe BEnv) -- ^ The resulting action
lookupKey h c = do
  m <- readIORef $ h^.stack
  f <- readIORef $ h^.baseL

  pure $ case m ^? L.atKey c of
    Nothing -> m ^? L.inLayer f c
    benv    -> benv
