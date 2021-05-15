module KMonad.Util.FFI
  ( FailCode
  , FFIResult
  , ffiReturn
  , onErr
  )
where

import KMonad.Prelude
import Foreign.C.Types (CInt)

--------------------------------------------------------------------------------
-- $ffi
--
-- NOTE: This is almost exactly the same as ExitCode from @base@, but that is
-- used to signal success/failure of the actual haskell program, so we just do a
-- little bit of duplication to disambiguate.

-- | Failures are encodes as integers (except for 0)
type FailCode = Int

-- | The result of some FFI call.
data FFIResult = FFISuccess | FFIFailure FailCode

-- | We always use 0 to signify success, anything else is a failure
ffiReturn :: CInt -> FFIResult
ffiReturn 0 = FFISuccess
ffiReturn n = FFIFailure $ fi n

-- | Helper function to throw some haskell-error when the FFI fails
--
-- >>> someFFIcall `onErr` \n -> _DidNotWork # n "The snozzle did not jig the glorp"
--
onErr :: (MonadUnliftIO m)
  => m FFIResult
  -> (Int -> m ())
  -> m ()
onErr a f = a >>= \case
  FFISuccess   -> pure ()
  FFIFailure n -> f n
