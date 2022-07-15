-- |

module K.App (IO, begin)
where

import K.Shell
import K.Layers

-- | Start KMonad.
begin :: IO ()
begin = inCtx ivkM doTask

-- | Choose what to do based on the 'task' setting in the 'ShellCfg'
doTask :: IvkM ()
doTask = view task >>= \case
  -- Run any task that does not need anything more than an invocation
  SendMsg -> devFail "not implemented yet"
  x -> inCtx cfgM $ case x of
    -- Run any task that does not need KeyIO
    CfgTest -> cfgTest
    y -> inCtx acqM $ case y of
      -- Run any task that needs all shell capacities
      FullRun -> loop
      EvTest  -> discover

-- | The standard 'do the remapping' task
loop :: AcqM ()
loop = do
  forever $ do
    e <- waitKioEvent
    atInfo $ pp e
    sendKioEvent e

-- | Print information about all events that arrive at kmonad
discover :: AcqM ()
discover = do
  logError "behold"

-- | Load all the configuration and then exit
cfgTest :: CfgM ()
cfgTest = do
  logError "Boom watchugot watchuwatchugot watchugot Booooooooom"
  -- pPrint =<< loadKbdFile =<< view kbdPath
