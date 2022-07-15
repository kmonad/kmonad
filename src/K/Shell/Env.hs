-- |

module K.Shell.Env
  ( ivkM
  , cfgM
  , acqM

  , inCtx

  , testIvkM
  , module K.Shell.Env.Initial
  )
where

import K.Shell.Env.Initial
import K.Shell.Error

import Control.Monad.Catch

-- | Run some RIO action in some Ctx
inCtx :: UIO m => Ctx a m env -> RIO env a -> m a
inCtx c r = runContT c $ inRIO r

-- | The different capacity levels the shell monad can run at

-- | Lowest capacity level
-- 1. access to cfg defaults overwritten by invoc
-- 2. running in a top-level exception handler
-- 3. access to a logging environment (i.e. can call logging actions)
ivkM :: (UIO m, MonadCatch m)
  => Ctx r m IvkCtxEnv
ivkM = do
  withShellHandler
  i <- withInvoc
  let c = defShellCfg ^. changed i
  l <- withLogging c
  ContT $ \f -> f (IvkCtxEnv c l i)

-- | Like 'ivkM' but parse invocation from text instead
--
-- Only to be used for debugging purposes to get run an 'IvkM' in the REPL
-- without having to run kmonad from a terminal.
testIvkM :: (UIO m, MonadCatch m)
  => Text -> Ctx r m IvkCtxEnv
testIvkM t = do
  withShellHandler
  let i = testInvoc t
  let c = defShellCfg ^. changed i
  l <- withLogging c
  ContT $ \f -> f (IvkCtxEnv c l i)

-- | Medium capacity level
-- 4. access to cfg default overwritten by cfgfile overwritten by invoc
-- 5. since locale is cfg-file-only, locale is now available
cfgM :: (UIO m, MonadCatch m, MonadReader e m, HasIvkCtxEnv e, HasLogEnv e)
  => Ctx r m CfgCtxEnv
cfgM = do
  i <- view $ ivkCtxEnv.invoc
  f <- loadCfgFile =<< view (ivkCtxEnv.shellCfg.cfgPath)
  let c = defShellCfg ^. changed f . changed i
  l <- withLogging c
  ContT $ \f -> f (CfgCtxEnv c l)

-- | Full capacity
-- 6. access to an acquired keysource, sink, and repeat environment
acqM :: (UIO m, MonadCatch m, MonadReader e m, HasCfgCtxEnv e, HasLogEnv e)
  => Ctx r m AcqCtxEnv
acqM = do
  c <- view $ cfgCtxEnv . shellCfg
  l <- view $ cfgCtxEnv . logEnv
  k <- withKio (c^.kioCfg)
  ContT $ \f -> f (AcqCtxEnv c l k)
