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
               , Program (..)
               , stringToProgram
               , progToSExprs
               ) where



-- | Set how many spaces an expanded tab takes.
tabSize = 4



type Indent = Int

-- | Hold Indent and Line in a data type
data IndentedLine = IndLine Indent String
                      deriving (Eq)

instance Show IndentedLine where
  show (IndLine n line) = [ ' ' | _ <- [1..n] ] ++ line

-- | Returns the indent.
indent ::  IndentedLine -> Indent
indent (IndLine n _) = n

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

-- | Checks if a line is empty.
isEmptyIndentedLine :: IndentedLine -> Bool
isEmptyIndentedLine (IndLine _ line) = null line



-- | Hold a program as a list of Expressions.
data Program = Prog [Expression]
  deriving (Eq)

instance Show Program where
  show (Prog [])     = ""
  show (Prog (x:xs)) = show x ++ "\n" ++ show (Prog xs)

-- | An Expression will later become a S-Expression.
data Expression = Expr IndentedLine [Expression]
                | ExprDeeper Expression
                | ExprsDeeper [Expression]
  deriving (Eq)

instance Show Expression where
  show (Expr x ys)      = show x ++ "\n" ++ (concat $ map (\ l -> l ++ "\n") $ map show ys)
  show (ExprDeeper x)   = show x ++ "\n"
  show (ExprsDeeper xs) = concat $ map (\ x -> x ++ "\n") $ map show xs

-- | Transform String to IndentedLines.
-- Remove empty lines.
-- Wrap lines in the Program data type and turn IndentedLines into Expressions.
-- Fix ExprDeeper to ExprsDeeper.
stringToProgram :: String -> Program
stringToProgram s = p1
  where ls = filter isNotEmptyIndentedLine $ map toIndentedLine $ lines s
          where isNotEmptyIndentedLine = not . isEmptyIndentedLine
        p0 = Prog $ toExpressions 0 ls []
        p1 = extendProgramDeeper p0

toExpressions :: Indent -> [IndentedLine] -> [IndentedLine] -> [Expression]
toExpressions _ []     []     = []
toExpressions _ []     (y:ys) = [ Expr y (toExpressions (nextIndent ys) ys []) ]
toExpressions n (x:xs) []
  | n == ind = toExpressions n   xs [ x ]
  | n <  ind = toExpressions n   xs [ x ]
  | n >  ind = toExpressions ind xs [ x ] -- throw error here?
  where ind = indent x :: Indent
toExpressions n (x:xs) (y:ys)
  | n == ind = Expr y (toExpressions (nextIndent ys) ys []) : toExpressions n xs [ x ]
  | n <  ind = toExpressions n xs ((y:ys) ++ [ x ])
  | n >  ind = ExprDeeper (Expr y (toExpressions (nextIndent ys) ys [])) : toExpressions ind xs [ x ]
  where ind    = indent x :: Indent

-- | Helper-Function for toExpressions
nextIndent :: [IndentedLine] -> Indent
nextIndent []     = -1
nextIndent (l:ls) = indent l


-- | Wrapper for extending further indentation upwards in an expression.
extendProgramDeeper :: Program -> Program
extendProgramDeeper p = mapProg (\ x -> extendExprDeeper x []) p
  where mapProg :: ([Expression] -> [Expression]) -> Program -> Program
        mapProg f (Prog xs) = Prog (map (mapExprs f) xs)

        mapExprs :: ([Expression] -> [Expression]) -> Expression -> Expression
        mapExprs f (Expr x ys)      = Expr x (f $ map (mapExprs f) ys)
        mapExprs f (ExprDeeper y)   = ExprDeeper (head $ f $ [ mapExprs f y ])
        mapExprs f (ExprsDeeper ys) = ExprsDeeper (f $ map (mapExprs f) ys)

-- | Further indentation to symbolize an extra parenthesis pair is extended upwards in the
-- expression, as toExpressions does not handle this.
-- Cannot be used on Expressions of the form ExprsDeeper.
extendExprDeeper :: [Expression] -> [Expression] -> [Expression]
extendExprDeeper []                  ys = ys
extendExprDeeper ((ExprDeeper x):xs) ys = extendExprDeeper xs [ deepX ]
  where deepX = (ExprsDeeper (ys ++ [ x ]))
extendExprDeeper (x             :xs) ys = extendExprDeeper xs (ys ++ [ x ])



progToSExprs :: Program -> String
progToSExprs (Prog []) = ""
progToSExprs (Prog (x:xs)) = exprToSExpr x ++ "\n" ++ progToSExprs (Prog xs)

exprToSExpr :: Expression -> String
exprToSExpr (Expr (IndLine _ f) xs) = "(" ++ f ++ (concat $ map ((" "++) . exprToSExpr) xs) ++ ")"
exprToSExpr (ExprDeeper x)          = "(" ++ exprToSExpr x ++ ") "
exprToSExpr (ExprsDeeper (x:xs))    = "(" ++ exprToSExpr x ++
                                        (concat $ map ((" "++) . exprToSExpr) xs) ++ ")"
