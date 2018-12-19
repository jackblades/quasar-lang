-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lexer where

--
import Prelude
import           Control.Applicative     ((<|>))
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Char as Tpc
import qualified Text.Parsec as Tp
import qualified Data.Text as T
import           Text.Parsec.Expr (Operator(..), Assoc(..))
import Text.Parsec.Pos (newPos)
import           Data.Functor.Identity (Identity)

import AST 
import           Debug.Trace (trace)
import Control.Monad (mzero)


nums = "0123456789"
langwords = "^`\'\"#~@:/%()[]{}"
spcs = " \n\r\t,"

lexer = P.makeTokenParser style
style :: P.LanguageDef st
style = emptyDef
    { P.commentStart   = "--|"
    , P.commentEnd     = "|--"
    , P.commentLine    = "--"
    , P.nestedComments = True
    , P.identStart     = Tpc.noneOf (nums ++ langwords ++ spcs)
    , P.identLetter    = P.identStart style <|> Tpc.oneOf (nums ++ ".:")
    , P.opStart        = mzero
    , P.opLetter       = mzero
    , P.reservedOpNames= [] -- :
    , P.reservedNames  = []
    , P.caseSensitive  = True
    }

--
singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

nonReserved f p = p >>= \name -> 
    if T.unpack (f name) `elem` P.reservedNames style
        then Tp.unexpected ("reserved word " ++ show name)
        else return name

ident = do
    c <- P.identStart style
    cs <- Tp.many (P.identLetter style)
    return $ T.pack (c:cs)


src p = Src <$> Tp.getPosition <*> p <*> Tp.getPosition
spanSrc x y e = Src (_beg x) e (_end y)
noSrc e = Src noSrcPos e noSrcPos where noSrcPos = newPos "" (-1) (-1)  -- TODO

--
lexeme = P.lexeme lexer
lexsym = P.symbol lexer
bool = (lexsym "true" *> pure True) 
   <|> (lexsym "false" *> pure False)

charLiteral = P.charLiteral lexer
natural = P.natural lexer
unsignedFloat = P.float lexer
int = natural <|> (char '-' *> fmap negate natural)
float = unsignedFloat <|> (char '-' *> fmap negate unsignedFloat)
char = Tpc.char
text = fmap T.pack (P.stringLiteral lexer)
identifier = fmap T.pack (P.identifier lexer)
rawString = char 'r' *> text
symbol = T.cons <$> char ':' <*> identifier

rawOp = P.operator lexer
binary p assoc f = Infix (p >>= return . f) assoc

-- TODO use 'sepEndBy'
comma = P.comma lexer *> return ()
colon = P.colon lexer *> return ()
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer


-- parse primitive "" "true"
-- primitive
--   :: Tp.ParsecT String u Identity (TextExpr a)
-- primitive = QLiteral
--     <|> BOOL                    <$> Tp.try bool
--     <|> DOUBLE                  <$> Tp.try float
--     <|> INT                     <$> int
--     --
--     <|> CHAR                    <$> (P.charLiteral lexer)
--     <|> STRING                  <$> text
--     <|> RAWSTRING               <$> rawString  -- TODO competes with application
--     --
--     <|> SYMBOL                  <$> symbol
--     <|> VAR                     <$> qualifiedName  -- TODO maybe move to expr?


-- qLiteral = src . fmap QLiteral
-- qForm = src . fmap QForm
-- qList = src . fmap QList
-- qVector = src . fmap QVector
-- qMap = src . fmap QMap
-- qSet = src . fmap QSet
-- qLambda = src .: fmap QLambda where (.:) = (.).(.)
-- qMetadata = src .: fmap QMetadata where (.:) = (.).(.)
-- qRegex = src . fmap QRegex
-- qVarQuote = src . fmap QVarQuote
-- qHostExpr = src .: fmap QHostExpr where (.:) = (.).(.)
-- qTag = src .: fmap QTag
-- qDiscard = src . fmap QDiscard
-- qDispatch = src .: fmap QDispatch where (.:) = (.).(.)
-- qDeref = src . fmap QDeref
-- qQuote = src . fmap QQuote
-- qBacktick = src . fmap QBacktick
-- qUnquote = src . fmap QUnquote
-- qUnquoteSplicing = src . fmap QUnquoteSplicing
-- qGenSym = src . fmap QGenSym