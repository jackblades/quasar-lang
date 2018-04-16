module Main where

import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Tp
import Lexer
import Parser

main :: IO ()
main = parseFromFile (Tp.many1 assign2) "/Users/singhpdz/quasar/test/test1"

assign2 = (,) 
    <$> identifier
    <*> (lexsym "=" *> expr)
parseFromFile p fname
     = do input <- readFile fname
          print $ Tp.parse (P.whiteSpace lexer *> p) fname input
          return ()

