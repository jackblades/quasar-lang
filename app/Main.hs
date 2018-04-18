module Main where

import Text.Pretty.Simple (pPrint)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Tp
import Lexer
import Parser

main :: IO ()
main = parseFromFile expr "/Users/singhpdz/quasar/test/test1"
-- main = parseFromFile (Tp.many1 assign2) "/Users/singhpdz/quasar/test/test1"

assign2 = (,) 
    <$> identifier
    <*> (lexsym "=" *> expr)
parseFromFile p fname
     = do input <- readFile fname
          pPrint $ Tp.parse (P.whiteSpace lexer *> p) fname input
          return ()

