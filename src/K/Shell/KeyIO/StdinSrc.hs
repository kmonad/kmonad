-- |

module K.Shell.KeyIO.StdinSrc where

import K.Shell.KeyIO.Initial

-- error -----------------------------------------------------------------------

-- | Things that can go wrong with 'StdinSrc' devices

withStdinSrc :: (UIO m, CanLog env m) => (KeySnk -> m a) -> m a
withStdinSrc f = undefined
