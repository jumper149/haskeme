---------------------------------------------------------------------------------------------------
-- |
-- Module      : Haskeme
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

module Haskeme ( IndentedLine (..)
               , toIndentedLine
               ) where

tabSize = 4

type Indent = Int

data IndentedLine = IndLine Indent String
                      deriving (Eq)

instance Show IndentedLine where
  show (IndLine n line) = [ ' ' | _ <- [1..n] ] ++ line

data Block = Block [IndentedLine]
           | Block [Block]

toIndentedLine :: String -> IndentedLine
toIndentedLine line
  | head line == ' '  = toIndentedLineSpace (IndLine 0 line)
  | head line == '\t' = toIndentedLineTab   (IndLine 0 line)
  | otherwise         = IndLine 0 line

toIndentedLineSpace :: IndentedLine -> IndentedLine
toIndentedLineSpace (IndLine n line)
  | head line == ' '  = toIndentedLineSpace (IndLine (n + 1) (tail line))
  | head line == '\t' = error $ "Mixed indentation found in the line containing: '" ++
                          show (toIndentedLineTab (IndLine (n + tabSize) (tail line))) ++ "'"
  | otherwise         = (IndLine n line)

toIndentedLineTab :: IndentedLine -> IndentedLine
toIndentedLineTab (IndLine n line)
  | head line == ' '  = error $ "Mixed indentation found in the line containing: '" ++
                          show (toIndentedLineSpace (IndLine (n + 1) (tail line))) ++ "'"
  | head line == '\t' = toIndentedLineTab (IndLine (n + tabSize) (tail line))
  | otherwise         = (IndLine n line)

mainBlock :: String -> Block
mainBlock prog = Block $ map toIndentedLine (lines prog)

--toBlock :: Indent -> Block -> Block
