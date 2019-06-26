---------------------------------------------------------------------------------------------------
-- |
-- Module      : CartanSubalgebra
-- Description : Implements a Cartan Subalgebra on top of a Lie Algebra
-- Copyright   : (c) Felix Springer, 2019
-- License     : BSD3
-- Maintainer  : felixspringer149@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A Cartan Subalgebra can be defined on top of a Lie Algebra to later construct the Root Space,
-- which is relevant for classification of the Lie Algebra.
--
---------------------------------------------------------------------------------------------------

module Haskeme ( toLines
               ) where

-- | Splits program into lines
toLines :: String -> [String]
toLines prog = [prog]

