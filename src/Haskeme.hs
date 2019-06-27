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
               , Block (..)
               , mainBlock
               , sepBlocks
               , blockLstoBlocks
               , toSExpr
               ) where



tabSize = 4



type Indent = Int

data IndentedLine = IndLine Indent String
                      deriving (Eq)

instance Show IndentedLine where
  show (IndLine n line) = [ ' ' | _ <- [1..n] ] ++ line

-- | Returns the indent.
indent ::  IndentedLine -> Indent
indent (IndLine n _) = n

data Block = Block [IndentedLine]
           | Blocks [Block]
             deriving (Show, Eq)



-- | Transform a String (Line) into IndentedLine.
toIndentedLine :: String -> IndentedLine
toIndentedLine line
  | null line         = IndLine 0 ""
  | head line == ' '  = toIndentedLineSpace (IndLine 0 line)
  | head line == '\t' = toIndentedLineTab   (IndLine 0 line)
  | otherwise         = IndLine 0 line

toIndentedLineSpace :: IndentedLine -> IndentedLine
toIndentedLineSpace (IndLine n line)
  | null line         = IndLine 0 ""
  | head line == ' '  = toIndentedLineSpace (IndLine (n + 1) (tail line))
  | head line == '\t' = error $ "Mixed indentation found in the line containing: '" ++
                          show (toIndentedLineTab (IndLine (n + tabSize) (tail line))) ++ "'"
  | otherwise         = (IndLine n line)

toIndentedLineTab :: IndentedLine -> IndentedLine
toIndentedLineTab (IndLine n line)
  | null line         = IndLine 0 ""
  | head line == ' '  = error $ "Mixed indentation found in the line containing: '" ++
                          show (toIndentedLineSpace (IndLine (n + 1) (tail line))) ++ "'"
  | head line == '\t' = toIndentedLineTab (IndLine (n + tabSize) (tail line))
  | otherwise         = (IndLine n line)



seperatedBlock :: Block -> Bool
seperatedBlock (Block [])         = True
seperatedBlock (Block [l])        = True
seperatedBlock (Block [l0,l1])    = indent l0 < indent l1
seperatedBlock (Block (l0:l1:ls)) = ind0 < ind1 && (foldr (&&) True $ map (== ind1) $ map indent ls)
  where ind0 = indent l0
        ind1 = indent l1

-- | Change String to IndentedLines.
--   Remove empty lines.
--   Wrap lines into a Block.
mainBlock :: String -> Block
mainBlock prog = Block $ filter notNullInd $ map toIndentedLine (lines prog)
  where notNullInd :: IndentedLine -> Bool
        notNullInd (IndLine _ l) = not $ null l

-- BROKEN!!! from this point on
sepBlocks :: Indent -> Block -> Block -> [Block]
sepBlocks _ (Block [])     b          = [ b ]
sepBlocks n (Block (l:ls)) (Block ks)
  | indent l == n = rec : sepBlocks n (Block ls) (Block [ l ])
  | indent l > n  = sepBlocks n (Block ls) (Block (ks ++ [ l ]))
  | indent l < n  = prevBlocks : nextBlocks
  where prevBlocks = Blocks [ rec ]
        nextBlocks = sepBlocks (indent l) (Block ls) (Block [ l ])
        rec
          | null ks                   = Block ks
          | seperatedBlock (Block ks) = Block ks
          | otherwise                 = Blocks $ sepBlocks nextInd (Block ks) (Block [])
        nextInd = nextIndent (Block ks)

removeNullBlocks :: [Block] -> [Block]
removeNullBlocks bs = filter notNullBlock bs
  where notNullBlock (Block ls) = not $ null ls
        notNullBlock (Blocks bs) = not $ null bs

blockLstoBlocks :: [Block] -> Block
blockLstoBlocks bs = Blocks $ removeNullBlocks bs

nextIndent :: Block -> Indent
nextIndent (Block ((IndLine n _):ls)) = n


toSExpr :: Block -> String
toSExpr (Block ls) = concat $ map (parensAround . toStr) ls
toSExpr (Blocks bs) = parensAround $ concat $ map toSExpr bs

toStr :: IndentedLine -> String
toStr (IndLine _ s) = s ++ " "

parensAround :: String -> String
parensAround s = "(" ++ s ++ ")"








