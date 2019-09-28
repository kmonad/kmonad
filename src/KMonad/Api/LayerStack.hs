{-|
Module      : KMonad.Api.LayerStack
Description : A stateful implementation of a MapStack of button layers
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

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
import Data.Maybe (fromJust)
import UnliftIO

import KMonad.Core
import KMonad.Core.Parser.Token
import KMonad.Domain.Button
import KMonad.Domain.Effect (CanButton)

import qualified KMonad.Core.MapStack as S


--------------------------------------------------------------------------------

-- | A LayerStack object that manages overlapping sets of handlers
data LayerStack m = LayerStack
  { mapStack :: MVar (S.MapStack LayerId KeyCode (Button m)) }

-- | A ClassyLens style typeclass to describe 'having a LayerStack'
class MonadIO m => HasLayerStack r m where
  layerStack :: Lens' r (LayerStack m)

-- | Use a LayerStack to handle a single key event
handleWith :: MonadIO m => KeyEvent -> LayerStack m -> m ()
handleWith e ls = withSignal e $ \x -> do
  ms <- readMVar . mapStack $ ls
  case S.lookup (e^.keyCode) ms of
    Nothing -> return ()
    Just b  -> runButton b x

-- | Push a LayerID to the top of the stack NOTE: the monads do not necessarily
-- have to line up, maybe change this? FIXME: Add error handling and remove the
-- FromJust (or not... I can ensure there are no references to nonexistent
-- layers when I compile the Config)
pushLS :: MonadIO m => LayerId -> LayerStack m -> m ()
pushLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . fromJust . push lid

-- | Pop a LayerId from the stack
popLS :: MonadIO m => LayerId -> LayerStack m -> m ()
popLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . pop lid


--------------------------------------------------------------------------------
-- Constructing a LayerStack from a set of tokens

-- | Turn a nested set of tokens into a layer stack of operations
mkLayerStack :: (CanButton m, MonadIO n)
  => [(LayerId, [(KeyCode, ButtonToken)])]
  -> LayerId
  -> n (LayerStack m)
mkLayerStack ts def = do
  -- There is probably a much prettier lensy way of doing this
  bs <- mapM (mapTup (mapM (mapTup encode))) ts
  h  <- newMVar . fromJust . push def . mkMapStack $ bs
  return $ LayerStack h
  where
    mapTup f (c, a) = (c,) <$> f a
