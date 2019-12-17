{-|
Module      : KMonad.Api.LayerStack
Description : A stateful implementation of a MapStack of button layers
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The 'LayerStack' object manages a stateful MapStack of layers that can be
manipulated. Press events are dispatched based on the current state of the
MapStack. Release events, however, are managed in such a way that it doesn't
matter which layer-state we are in now, a release will always correspond to the
release of the button which initiated the press. This is to prevent situations
where you press an 'a' in a layer where that is mapped to, for example @emit q@,
switch to a different layer, release the 'a', but catch that release with a
different handler, resulting in a permanently pressed @q@.

-}
module KMonad.Api.LayerStack
  ( LayerStack
  , mkLayerStack
  , HasLayerStack(..)
  , handleWith
  , pushLS
  , popLS
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)
import UnliftIO

import KMonad.Core
import KMonad.Core.Parser.Token
import KMonad.Domain.Button
import KMonad.Domain.Effect

import qualified KMonad.Core.MapStack as S
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T


--------------------------------------------------------------------------------

-- | A LayerStack object that manages overlapping sets of handlers
data LayerStack m = LayerStack
  { mapStack   :: MVar (S.MapStack Name KeyCode (Button m))
  , mapRelease :: MVar (M.HashMap KeyCode (Button m))}

-- | A ClassyLens style typeclass to describe 'having a LayerStack'
class MonadIO m => HasLayerStack r m where
  layerStack :: Lens' r (LayerStack m)

-- | Use a LayerStack to handle a single key event
handleWith :: (MonadButton m, MonadIO m) => KeyEvent -> LayerStack m -> m ()
handleWith e ls
  -- Handle presses by looking up the key in the layerstack
  | isPress e = do
      let kc = e^.keyCode
      ms  <- readMVar . mapStack $ ls
      case S.lookup (e^.keyCode) ms of
        Nothing -> return ()
        Just b  -> do
          liftIO . modifyMVar_ (mapRelease ls) $ return . M.insert kc b
          press b

  -- Handle releases by finding the key that initiated the press
  | isRelease e = do
      let kc = e^.keyCode
      rels <- takeMVar (mapRelease ls)
      case M.lookup kc rels of
        Nothing -> putMVar (mapRelease ls) rels
        Just b  -> do
          putMVar (mapRelease ls) (M.delete kc rels)
          release b

  -- This should be unreachable
  | otherwise = pure ()


myFromJust :: String -> Maybe a -> a
myFromJust = fromMaybe . error

-- | Push a LayerID to the top of the stack NOTE: the monads do not necessarily
-- have to line up, maybe change this? FIXME: Add error handling and remove the
-- FromJust (or not... I can ensure there are no references to nonexistent
-- layers when I compile the Config)
pushLS :: MonadIO m => Name -> LayerStack m -> m ()
pushLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . myFromJust ("Cannot find layer: " <> T.unpack lid) . push lid

-- | Pop a Name from the stack
popLS :: MonadIO m => Name -> LayerStack m -> m ()
popLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . pop lid


--------------------------------------------------------------------------------
-- Constructing a LayerStack from a set of tokens

-- | Turn a nested set of tokens into a layer stack of operations
mkLayerStack :: (CanButton m, MonadIO n)
  => [(Name, [(KeyCode, ButtonToken)])]
  -> Name
  -> n (LayerStack m)
mkLayerStack ts def = do
  -- There is probably a much prettier lensy way of doing this
  bs <- mapM (mapTup (mapM (mapTupB encode))) ts
  h  <- newMVar . myFromJust "making layer error" . push def . mkMapStack $ bs
  LayerStack h <$> newMVar (M.empty)
  where
    mapTup  f (c, a) = (c,) <$> f a
    mapTupB f (c, a) = (c,) <$> f c a
