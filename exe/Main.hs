---------------------------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : Compiler from I- to S-Expressions for the Scheme Programming Language
-- Copyright   : (c) Felix Springer, 2019
-- License     : BSD3
-- Maintainer  : felixspringer149@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is a compiler from I- to S-Expressions for the Scheme Programming Language.
--
---------------------------------------------------------------------------------------------------

module Main where

import System.Console.GetOpt
import System.Environment (getArgs)

import Haskeme



data Options = Options { optInput :: IO String
                       , optOutput :: String -> IO ()
                       }

defaultOptions :: Options
defaultOptions = Options { optInput  = getContents
                         , optOutput = putStr
                         }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['i'] ["input"]
              (ReqArg
                (\ arg opt -> return opt { optInput = readFile arg })
                "FILE")
              "Input FILE"
          , Option ['o'] ["output"]
              (ReqArg
                (\ arg opt -> return opt { optOutput = writeFile arg })
                "FILE")
              "Output FILE"
          ]



main :: IO ()
main = do
         args <- getArgs

         let (optArgs , nonOpts , errs) = getOpt Permute options args

         opts <- foldl (>>=) (return defaultOptions) optArgs

         let Options { optInput  = input
                     , optOutput = output
                     } = opts

         inputString <- input
         output $ (progToSExprs . stringToProgram) inputString
