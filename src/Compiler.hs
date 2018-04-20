module Compiler where

import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Tp
import Lexer
import Parser
    
-- ns
parseNs = (,) <$> ns <*> semiSep assign where
    ns = lexsym "ns" *> qualifiedName
    assign = (,)  <$> identifier <*> (lexsym "="  *> expr)

parseFromFile p fname
    = do input <- readFile fname
         return $ Tp.parse (P.whiteSpace lexer *> p) fname input

