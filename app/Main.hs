module Main where

import Compiler
import Text.Pretty.Simple (pPrint)
import System.Environment (getArgs)

main :: IO ()
main = do
    (file:_) <- getArgs
    parseFromFile parseNs file >>= pPrint
