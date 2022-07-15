{- | TODO: insert header

A simple way of encoding a 'KeyRepeatCfg' in 'Text'.

A KeyRepeat expression follows one of the following patterns:
@sim:500:200@
@echo:@
@pass:@

TODO: expand documentation about linux evdev-path options

-}
module K.Shell.Cfg.Expr.KeyRepeat
  ( keyRepeatExpr )
where

import K.Shell.Cfg.Expr.Initial

-- | An expression relating 'KeyRepeatCfg' to 'Text'
keyRepeatExpr :: Expr KeyRepeatCfg
keyRepeatExpr = customExpr "KeyRepeat" keyRepeatT keyRepeatP

-- | How to parse a 'KeyRepeatCfg' from 'Text'
keyRepeatP :: Parser KeyRepeatCfg
keyRepeatP = do
  -- Parse "sim:300:100" pattern
  let simP = do d <- string "sim:" *> msP
                r <- string ":"    *> msP
                pure . Simulate $ DelayRate d r
  -- Parse any valid pattern
  choice [ simP
         , EchoOS       <$ string "echo:" $> EchoOS
         , IgnoreRepeat <$ string "pass:" $> IgnoreRepeat ]

-- | How to print a 'KeyRepeatCfg' to 'Text'
keyRepeatT :: Printer KeyRepeatCfg
keyRepeatT EchoOS       = "echo:"
keyRepeatT IgnoreRepeat = "pass:"
keyRepeatT (Simulate (DelayRate d r))
  = "sim:" <> tshow (d^.ms) <> ":" <> tshow (r^.ms)
