{-# LANGUAGE NoFieldSelectors #-}
-- No explicit export list, since we only define a data structure,
-- which we want to fully export
module KMonad.Model.Cfg where

import KMonad.Keyboard
import KMonad.Model.Button
import KMonad.Model.Keymap
import KMonad.Model.Parsing
import KMonad.Model.TH

data Stage
  = Parsing  -- ^ While parsing, before joining
  | Token    -- ^ A joined config
  | Joining  -- ^ An intermediate config used during joining
  | Cmd      -- ^ For processing command line arguments
  | App      -- ^ To start the app
  | Env      -- ^ While running

type family XSource x where
  XSource 'Parsing  = [IToken]
  XSource 'Token    = LogFunc -> IO (Acquire KeySource)
  XSource 'Joining  = ()
  XSource 'Cmd      = Maybe IToken
  XSource 'App      = Acquire KeySource
  XSource 'Env      = KeySource

type family XSink x where
  XSink 'Parsing  = [OToken]
  XSink 'Token    = LogFunc -> IO (Acquire KeySink)
  XSink 'Joining  = ()
  XSink 'Cmd      = Maybe OToken
  XSink 'App      = Acquire KeySink
  XSink 'Env      = KeySink

type family XLogging x where
  XLogging 'Cmd  = LogLevel
  XLogging 'Env  = LogFunc
  XLogging _     = ()

type family XCmpKey x where
  XCmpKey 'Parsing  = [DefButton]
  XCmpKey 'Joining  = Maybe Button
  XCmpKey 'Cmd      = Maybe DefButton
  XCmpKey _         = ()

type family XCmpSeqDelay x where
  XCmpSeqDelay 'Parsing  = [Int]
  XCmpSeqDelay 'Joining  = Maybe Int
  XCmpSeqDelay 'Cmd      = Maybe Int
  XCmpSeqDelay _         = ()

type family XKeymap x where
  XKeymap 'Parsing  = [KExpr]
  XKeymap 'Token    = LMap Button
  XKeymap 'Joining  = [KExpr]
  XKeymap 'Cmd      = FilePath
  XKeymap 'App      = LMap Button
  XKeymap 'Env      = Keymap

type family XFstLayer x where
  XFstLayer 'Token  = LayerTag
  XFstLayer 'App    = LayerTag
  XFstLayer _       = ()

type family XFallThrough x where
  XFallThrough 'Parsing  = [Bool]
  XFallThrough 'Joining  = ()
  XFallThrough 'Cmd      = Maybe Bool
  XFallThrough _         = Bool

type family XAllowCmd x where
  XAllowCmd 'Parsing  = [Bool]
  XAllowCmd 'Joining  = ()
  XAllowCmd 'Cmd      = Maybe Bool
  XAllowCmd _         = Bool

type family XStartDelay x where
  XStartDelay 'Cmd  = Milliseconds
  XStartDelay 'App  = Milliseconds
  XStartDelay _     = ()

type family XKeySeqDelay x where
  XKeySeqDelay 'Parsing  = [Int]
  XKeySeqDelay 'Token    = Maybe Int
  XKeySeqDelay 'Joining  = ()
  XKeySeqDelay 'Cmd      = Maybe Int
  XKeySeqDelay 'App      = Maybe Milliseconds
  XKeySeqDelay 'Env      = ()

type family XImplArnd x where
  XImplArnd 'Parsing  = [ImplArnd]
  XImplArnd 'Joining  = ImplArnd
  XImplArnd 'Cmd      = Maybe ImplArnd
  XImplArnd _         = ()

data Cfg x = Cfg
  { _source       :: XSource x
  , _sink         :: XSink x
  , _logging      :: XLogging x
  , _cmpKey       :: XCmpKey x
  , _cmpSeqDelay  :: XCmpSeqDelay x
  , _keymap       :: XKeymap x
  , _fstLayer     :: XFstLayer x
  , _fallThrough  :: XFallThrough x
  , _allowCmd     :: XAllowCmd x
  , _startDelay   :: XStartDelay x
  , _keySeqDelay  :: XKeySeqDelay x
  , _implArnd     :: XImplArnd x
  }
makeClassy ''Cfg
mkStages ''Cfg

deriving instance Show PCfg
deriving instance Eq PCfg

instance Semigroup PCfg where
  PCfg is os cks csds km fts acs ksds ias <> PCfg is' os' cks' csds' km' fts' acs' ksds' ias' =
    PCfg (is ++ is') (os ++ os') (cks ++ cks') (csds ++ csds') (km ++ km') (fts ++ fts') (acs ++ acs') (ksds ++ ksds') (ias ++ ias')

instance Monoid PCfg where mempty = PCfg [] [] [] [] [] [] [] [] []
