{- | TODO: insert header

A simple way of encoding a 'KeyOutputCfg' in 'Text'.

A KeyOutput expression follows one of the following patterns:
@lin:optional-uinput-name@
@cmd:cmd-expr-to-execute@
@win:@
@mac:@
@stdout:@

TODO: expand documentation

-}

module K.Shell.Cfg.Expr.KeyOutput
  ( keyOutputExpr )
where

import K.Shell.Cfg.Expr.Initial
import K.Shell.Cfg.Expr.Cmd
import K.Shell.Cfg.Expr.Path

-- | An expression relating 'KeyOutputCfg' to 'Text'
keyOutputExpr :: Expr KeyOutputCfg
keyOutputExpr = customExpr "KeyOutput" outputT outputP

-- | How to parse a 'KeyOutputCfg' from 'Text'
outputP :: Parser KeyOutputCfg
outputP = choice
  [ LinUinputSnk <$> (string "lin:" *> maybeRestP)
  , CmdSnk       <$> (string "cmd:" *> cmdExpr^.parser)
  , WinSendSnk   <$ string "win:"
  , MacKextSnk   <$ string "mac:"
  , StdoutSnk    <$ string "stdout:"
  ]

-- | How to print a 'KeyOutputCfg' to 'Text'
outputT :: Printer KeyOutputCfg
outputT (LinUinputSnk mt) = "lin:" <> fromMaybe "" mt
outputT (CmdSnk c)        = "cmd:" <> encode cmdExpr c
outputT WinSendSnk        = "win:"
outputT MacKextSnk        = "mac:"
outputT StdoutSnk         = "stdout:"
