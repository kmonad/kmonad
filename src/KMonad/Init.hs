module KMonad.Init where

import Prelude

import KMonad.Runner
import KMonad.Util


-- newtype Init r e a = Init { unInit :: ContT r (RIO e) a }

-- mkInit :: (a -> RIO e r) -> Init r e a
-- mkInit = Init . ContT

type Init r e a = ContT r (RIO e) a

withThreadC :: HasLogFunc e => Name -> RIO e a -> Init r e ()
-- withThreadC n a = ContT (\next -> withThread n a (next ()))
withThreadC n a = ContT (\next -> withThread n a (next ()))
