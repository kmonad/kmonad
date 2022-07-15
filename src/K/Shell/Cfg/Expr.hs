-- |

module K.Shell.Cfg.Expr
  ( -- * Basic types and operations
    Expr
  , ExprError
  , AsExprError(..)
  , decode
  , decoded
  , encode

    -- * Simple expressions
  , boolExpr
  , taskExpr
  , logLevelExpr
  , logColorExpr
  , cmdAllowExpr

    -- * Complex expressions
    -- ** Path
  , module K.Shell.Cfg.Expr.Path
    -- ** Cmd
  , module K.Shell.Cfg.Expr.Cmd
    -- ** KeyIO
  , module K.Shell.Cfg.Expr.KeyInput
  , module K.Shell.Cfg.Expr.KeyOutput
  , module K.Shell.Cfg.Expr.KeyRepeat
    -- ** Toggle sequences
  , module K.Shell.Cfg.Expr.Toggles

  )
where

import K.Shell.Cfg.Expr.Initial
import K.Shell.Cfg.Expr.Cmd
import K.Shell.Cfg.Expr.KeyInput
import K.Shell.Cfg.Expr.KeyOutput
import K.Shell.Cfg.Expr.KeyRepeat
import K.Shell.Cfg.Expr.Path
import K.Shell.Cfg.Expr.Toggles

-- simple expressions ----------------------------------------------------------

-- | An expression binding @on@ and @off@ to 'True' and 'False' respectively
boolExpr :: Expr Bool
boolExpr = namedExpr "Bool"
  [ ("off", False)
  , ("on", True)
  ]

-- | An expression binding names to the 'Task' values
taskExpr :: Expr Task
taskExpr = namedExpr "Task"
  [ ("run", FullRun)
  , ("test", CfgTest)
  , ("discover", EvTest)
  ]

-- | An expression binding names to the 4 'LogLevel' values
logLevelExpr :: Expr LogLevel
logLevelExpr = namedExpr "LogLevel"
  [ ("error", LevelError)
  , ("warn", LevelWarn)
  , ("info", LevelInfo)
  , ("debug", LevelDebug)
  ]

-- | An expression binding names to different 'LogLevel' values
logColorExpr :: Expr LogColor
logColorExpr = namedExpr "LogColor"
  [ ("dark-bg" , DarkBG)
  , ("light-bg", LightBG)
  , ("none"    , Monochrome)
  ]

-- | An expression binding names to the different 'CmdAllow' values
cmdAllowExpr ::  Expr CmdAllow
cmdAllowExpr = namedExpr "CmdAllow"
  [ ("none", NoCmds)
  , ("init", InitCmds)
  , ("all", AllCmds)
  ]
