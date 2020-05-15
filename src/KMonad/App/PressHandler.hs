{-|
Module      : KMonad.App.PressHandler
Description : Implementation of mapping key-presses to button actions
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

In KMonad we handle all releases (and some other actions) via callback
mechanisms. It is the button-presses that get handled through a keymap. It is
the 'PressHandler' component that manages the keymap state and ensures that
incoming events are mapped to

-}
module KMonad.App.PressHandler
  ( PressHandler
  , mkPressHandler
  , layerOp
  , lookupKey
  )
where


import KPrelude

import KMonad.Button hiding (layerOp)
import KMonad.Keyboard

import qualified Data.LayerStack as Ls

--------------------------------------------------------------------------------
-- $env
--

-- | The 'PressHandler' environment containing the current keymap
--
-- NOTE: Since the 'PressHandler' will never have to deal with anything
-- asynchronously we can simply use 'IORef's here.
data PressHandler = PressHandler
  { _keymap :: IORef (Keymap BEnv)
  , _baseL  :: IORef LayerTag
  }
makeClassy ''PressHandler

-- | Create a 'PressHandler' from a 'Keymap' of uninitialized 'Button's and a
-- tag indicating which layer should start as the base.
mkPressHandler' :: MonadUnliftIO m
  => LayerTag         -- ^ The initial base layer
  -> Keymap Button    -- ^ The keymap of 'Button's
  -> m PressHandler
mkPressHandler' n m = do
  envs <- m & Ls.items . itraversed %%@~ \(_, c) b -> initBEnv b c
  PressHandler <$> newIORef envs <*> newIORef n

-- | Create a 'PressHandler' but do so in the context of a 'ContT' monad to ease nesting.
mkPressHandler :: MonadUnliftIO m => LayerTag -> Keymap Button -> ContT r m PressHandler
mkPressHandler n = lift . mkPressHandler' n


--------------------------------------------------------------------------------
-- $op
--
-- The following code describes how we add and remove layers from the
-- 'PressHandler'.

-- | Print a header message followed by an enumeration of the layer-stack
debugReport :: HasLogFunc e => PressHandler -> Utf8Builder -> RIO e ()
debugReport h hdr = do
  st <- view Ls.stack <$> (readIORef $ h^.keymap)
  let ub = foldMap (\(i, n) -> " "  <> display i
                            <> ". " <> display n <> "\n")
             (zip ([1..] :: [Int]) st)
  ls <- readIORef (h^.baseL)
  logDebug $ hdr <> "\n" <> ub <> "Base-layer: " <> display ls <> "\n"

-- | Perform operations on the layer-stack
layerOp :: (HasLogFunc e)
  => PressHandler -- ^ The 'PressHandler' environment
  -> LayerOp      -- ^ The 'LayerOp' to perform
  -> RIO e ()     -- ^ The resulting action
layerOp h o = let km = h^.keymap in case o of
  (PushLayer n) -> do
    Ls.pushLayer n <$> readIORef km >>= \case
      Left e   -> throwIO e
      Right m' -> writeIORef km m'
    debugReport h $ "Pushed layer to stack: " <> display n

  (PopLayer n) -> do
    Ls.popLayer n <$> readIORef km >>= \case
      Left e   -> throwIO e
      Right m' -> writeIORef km m'
    debugReport h $ "Popped layer from stack: " <> display n

  (SetBaseLayer n) -> do
    (n `elem`) . view Ls.maps <$> (readIORef km) >>= \case
      True  -> writeIORef (h^.baseL) n
      False -> throwIO $ Ls.LayerDoesNotExist n
    debugReport h $ "Set base layer to: " <> display n


--------------------------------------------------------------------------------
-- $run
--
-- How we use the 'PressHandler' to handle events.

-- | Lookup the 'BEnv' currently mapped to the key press.
lookupKey :: MonadIO m
  => PressHandler   -- ^ The 'PressHandler' to lookup in
  -> Keycode        -- ^ The 'Keycode' to lookup
  -> m (Maybe BEnv) -- ^ The resulting action
lookupKey h c = do
  m <- readIORef $ h^.keymap
  f <- readIORef $ h^.baseL

  pure $ case m ^? Ls.atKey c of
    Nothing -> m ^? Ls.inLayer f c
    benv    -> benv
