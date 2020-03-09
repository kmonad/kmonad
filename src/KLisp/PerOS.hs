{-# LANGUAGE CPP #-}
{-|
Module      : KLisp.PerOS
Description : Bits of parsing that differ between Windows and Linux
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KLisp.PerOS

where

import KPrelude


-- Linux only imports
#ifdef linux_HOST_OS
#endif

-- Windows only import
#ifdef mingw32_HOST_OS
#endif
