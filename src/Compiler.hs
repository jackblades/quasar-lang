{-# LANGUAGE TupleSections #-}

module Compiler where

import AST
import           Control.Applicative     ((<|>))
import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Tp
import Lexer
import Parser
import Data.Text as T
    
-- ns
parseNs = (,) <$> ns <*> semiSep assign where
    ns = lexsym "ns" *> qualifiedName
    assign = Tp.try ((,) <$> identifier <*> (lexsym "="  *> expr))
         <|> fmap ((T.pack "ERROR" ,) . ERROR) (T.pack <$> Tp.many1 (Tp.noneOf ";"))

parseFromFile p fname
    = do input <- readFile fname
         return $ Tp.parse (P.whiteSpace lexer *> p) fname input

