{-|
Module      : KLisp.Joiner
Description : The code that turns tokens into a DaemonCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KList.Joiner

where

import KPrelude hiding (uncons)

import KLisp.Parser

import KMonad
import KMonad.Daemon
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Keyboard.IO.Linux.UinputSink

import RIO.List (uncons)
import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- $err

data CfgError
  = DuplicateBlock Text
  | MissingBlock Text
  deriving Show

instance Exception CfgError

--------------------------------------------------------------------------------
-- $full

-- | Parse an entire DaemonCfg from a list of KExpr
joinConfig :: HasLogFunc e => [KExpr] -> RIO e DaemonCfg
joinConfig es = do

  -- Extract and join the DefIO block
  let dios = catMaybes . flip map es $ \case
        KDefIO x -> Just x
        _        -> Nothing
  dio <- case uncons dios of
    Just (x, []) -> pure x
    Just _       -> throwIO $ DuplicateBlock "DefIO"
    Nothing      -> throwIO $ MissingBlock   "DefIO"
  (o, i) <- joinIO dio

  -- Extract and join the DefSrc block
  let srcs = catMaybes . flip map es $ \case
        KDefSrc x -> Just x
        _         -> Nothing
  src <- case uncons srcs of
    Just (x, []) -> pure x
    Just _       -> throwIO $ DuplicateBlock "DefSrc"
    Nothing      -> throwIO $ MissingBlock   "DefSrc"

  -- Extract all the alias blocks
  let als = catMaybes . flip map es $ \case
        KDefAlias x -> Just x
        _           -> Nothing

  -- Extract all the deflayer blocks
  let lys = catMaybes . flip map es $ \case
        KDefLayer x -> Just x
        _           -> Nothing

  (km, fl) <- joinKeymap src als lys

  pure $ DaemonCfg
    { _keySinkDev   = o
    , _keySourceDev = i
    , _keymapCfg    = km
    , _firstLayer   = fl
    , _port         = ()
    }

--------------------------------------------------------------------------------
-- $io

joinIO :: HasLogFunc e => DefIO -> RIO e (Acquire KeySink, Acquire KeySource)
joinIO dio = do
  i <- case _itoken dio of
    KDeviceSource pth -> deviceSource64 pth
  o <- case _otoken dio of
    KUinputSink t init ->
      uinputSink (defUinputCfg { _keyboardName = T.unpack t
                               , _postInit     = T.unpack <$> init})
  pure (o, i)


--------------------------------------------------------------------------------
-- $kmap

joinKeymap :: HasLogFunc e
  => DefSrc
  -> [DefAlias]
  -> [DefLayer]
  -> RIO e (Keymap Button, LayerTag)
joinKeymap = undefined



--------------------------------------------------------------------------------
-- $test

fname :: String
fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

test :: IO [KExpr]
test = loadTokens fname
