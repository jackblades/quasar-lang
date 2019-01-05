{-# LANGUAGE TupleSections #-}

module Compiler where

import           Prelude hiding (maybe)
import           TextAST
import           Control.Applicative     ((<|>))
import qualified Text.Parsec.Token as P
import qualified Text.Parsec as Tp
import           TextASTLexer as L
import           TextASTParser
import           Data.Text as T

data ImportQ = ImportAs [T.Text] [T.Text]
             | FromImport [T.Text] [([T.Text], [T.Text])]
             deriving (Show, Eq, Ord)

maybe x p = p <|> pure x

--
importQ 
    -- usage: open control.monad
    =   lexsym "open" *> (FromImport <$> qualifiedName <*> pure [([T.pack "*"], [])])
    -- usage: import control.monad [as M]
    <|> ImportAs <$> (lexsym "import" *> qualifiedName) <*> (maybe [] $ as qualifiedName)
    -- usage: from control import monad as m, applicative as a
    <|> FromImport <$> (lexsym "from" *> qualifiedName) 
        <*> (lexsym "import" *> (maybe [] $ commaSep1 $ qnameAsQname [])) 
    where --
    asterisk = L.singleton $ L.singleton (T.pack <$> lexsym "*")
    as p = lexsym "as" *> p
    qnameAsQname x = (,) <$> qualifiedName <*> (maybe x $ as qualifiedName)
                          

parseNs = (,,) <$> ns <*> semiSep importQ <*> semiSep assign where  -- TODO multiple imports
    ns = lexsym "ns" *> qualifiedName
    assign = Tp.try ((,) <$> identifier <*> (lexsym "="  *> expr))
         <|> fmap (T.pack "ERROR" ,)
                  (fparser $ (ERROR . T.pack) <$> Tp.many1 (Tp.noneOf ";"))

parseFromFile p fname
    = do input <- readFile fname
         return $ Tp.parse (P.whiteSpace lexer *> p) fname input

