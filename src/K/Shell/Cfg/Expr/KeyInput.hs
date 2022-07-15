{- | TODO: insert header

A simple way of encoding a 'KeyInputCfg' in 'Text'.

A KeyInput expression follows one of the following patterns:
@lin:path-expr-to-evdev-file@
@mac:optional-name@
@cmd:cmd-expr-to-execute@
@win:@
@stdin:@

TODO: expand documentation about linux evdev-path options

-}
module K.Shell.Cfg.Expr.KeyInput
  ( keyInputExpr )
where


import K.Shell.Cfg.Expr.Initial
import K.Shell.Cfg.Expr.Cmd
import K.Shell.Cfg.Expr.Path

-- | An expression relating 'KeyInputCfg' to 'Text'
keyInputExpr :: Expr KeyInputCfg
keyInputExpr = customExpr "KeyInput" inputT inputP

-- | The 'Path' to an evdev file with some extra root shortcuts
evdevPath :: Expr Path
evdevPath = mkPathExpr "EvdevPath"
  [ ('v', Custom "/dev/input/")
  , ('i', Custom "/dev/input/by-id/") ]

-- | How to parse a 'KeyInputCfg' from 'Text'
inputP :: Parser KeyInputCfg
inputP = choice
  [ LinEvdevSrc <$> (string "lin:" *> evdevPath^.parser)
  , CmdSrc      <$> (string "cmd:" *> cmdExpr^.parser)
  , MacIOKitSrc <$> (string "mac:" *> maybeRestP)
  , WinHookSrc  <$ string "win:"
  , StdinSrc    <$ string "stdin:"
  ]

-- | How to print a 'KeyInputCfg' to 'Text'
inputT :: Printer KeyInputCfg
inputT (LinEvdevSrc f)  = "lin:" <> encode evdevPath f
inputT WinHookSrc       = "win:"
inputT (MacIOKitSrc mt) = "mac:" <> fromMaybe "" mt
inputT (CmdSrc c)       = "cmd:" <> encode cmdExpr c
inputT StdinSrc         = "stdin:"
