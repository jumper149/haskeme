---------------------------------------------------------------------------------------------------
-- |
-- Module      : CartanSubalgebra
-- Description : Implements functions for the Haskeme executable
-- Copyright   : (c) Felix Springer, 2019
-- License     : BSD3
-- Maintainer  : felixspringer149@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module implements the necessary functions for the Haskeme Main executable.
--
---------------------------------------------------------------------------------------------------

module Haskeme ( toLines
               ) where

-- | Splits program into lines
toLines :: String -> [String]
toLines prog = [prog]

