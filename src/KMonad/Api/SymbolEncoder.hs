module KMonad.Api.SymbolEncoder

where

import Control.Lens
import Control.Monad.Reader
import UnliftIO.MVar

import KMonad.Core
import KMonad.Domain.Button
import KMonad.Domain.Effect


import qualified Data.HashMap.Strict as M

type SymbolEncoder  = SpecialSymbol -> Maybe KeySequence
type DeadKeyEncoder = DeadKey       -> Maybe KeySequence

composeSymbol :: SymbolEncoder
composeSymbol ss = (mkKeyTap KeyRightAlt <>) <$> (ss^.composeSeq)

composeDeadKey :: DeadKeyEncoder
composeDeadKey dk = (mkKeyTap KeyRightAlt <>) <$> (dk^.composeSeq)

data SymbolEncoderRing = SymbolEncoderRing
  { _seEncoders :: M.HashMap Name SymbolEncoder
  , _currentSE  :: MVar SymbolEncoder
  , _dkEncoders :: M.HashMap Name DeadKeyEncoder
  , _currentDK  :: MVar DeadKeyEncoder
  }
makeClassy ''SymbolEncoderRing

mkSymbolEncoderRing :: MonadIO m
  => [(Name, SymbolEncoder)]
  -> [(Name, DeadKeyEncoder)]
  -> m SymbolEncoderRing
mkSymbolEncoderRing ses dks
  | null ses || null dks = error "Empty values in decoder ring"
  | otherwise = do
  let mse = M.fromList ses
  let mdk = M.fromList dks
  cse <- newMVar . snd . head $ ses
  cdk <- newMVar . snd . head $ dks
  pure $ SymbolEncoderRing
    { _seEncoders = mse
    , _currentSE  = cse
    , _dkEncoders = mdk
    , _currentDK  = cdk
    }

-- | Encode a symbol using the current encoder
encodeSymbol :: (HasSymbolEncoderRing r, MonadReader r m, MonadIO m)
  => SpecialSymbol         -- ^ The symbol to encode
  -> m (Maybe KeySequence) -- ^ The sequence encoding this symbol
encodeSymbol ss = view currentSE >>= readMVar >>= \f -> pure $ f ss

-- | Encode a deadkey using the current encoder
encodeDeadKey :: (HasSymbolEncoderRing r, MonadReader r m, MonadIO m)
  => DeadKey               -- ^ The DeadKey to encode
  -> m (Maybe KeySequence) -- ^ The sequence encoding this symbol
encodeDeadKey dk = view currentDK >>= readMVar >>= \f -> pure $ f dk
