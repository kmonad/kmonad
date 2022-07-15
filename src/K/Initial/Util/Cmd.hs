-- |

module K.Initial.Util.Cmd
  ( Cmd(..)
  , AsCmd(..)
) where


import K.Initial.Initial

-- | Different ways in which we can run a shell-command
data Cmd
  = SimpleCmd Text              -- ^ Simple string to be injected into shell
  | CompoundCmd FilePath [Text] -- ^ Command name and arguments
  | PassCmd                     -- ^ Do nothing
  deriving (Eq, Show)
makeClassyPrisms ''Cmd
