module KLisp.Utility

where

import KPrelude

import Text.Megaparsec


--------------------------------------------------------------------------------
-- $named
--
-- Turn an alist of [(txt, item)] into a parser of 'item'.



-- case, the parse state is not updated.
-- terminated :: Parser a -> Parser a
