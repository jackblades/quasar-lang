module Main where

import           Lib
import           Parser
import           Text.Parsec

main :: IO ()
main = play "32 + 44"

play :: String -> IO ()
play inp = case parse expr "" inp of
             { Left err  -> print err
             ; Right ans -> print ans
             }
