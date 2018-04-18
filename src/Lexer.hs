-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lexer where

--
import           Control.Applicative     ((<|>))
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Char as Tpc
import qualified Text.Parsec as Tp
import qualified Data.Text as T
import           Text.Parsec.Expr (Operator(..), Assoc(..))
import           Data.Functor.Identity (Identity)

import AST (Expr(UNIT, BOOL, INT, DOUBLE, CHAR, STRING, RAWSTRING, VAR, SYMBOL))
import           Debug.Trace (trace)

lexer = P.makeTokenParser style
style :: P.LanguageDef st
style = emptyDef
    { P.commentStart   = "-|"
    , P.commentEnd     = "|-"
    , P.commentLine    = "--"
    , P.nestedComments = True
    , P.identStart     = Tpc.lower
    , P.identLetter    = Tpc.alphaNum <|> Tpc.oneOf "_-'"
    , P.opStart        = P.opLetter style
    , P.opLetter       = Tpc.oneOf ":!#$%&*+./<=>?@\\^|-~" -- TODO '`'
    , P.reservedOpNames= ["\\", "//", "\\\\", "->", "=>", "<-", "%", "$"] -- :
    , P.reservedNames  = ["true", "false", "if", "then", "else"] --, "match", "do", "try", "catch", "finally", "throw"]
    , P.caseSensitive  = True
    }

--
singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

lname = fmap T.pack $ (:) <$> P.identStart style <*> Tp.many (P.identLetter style)
oplname = lname <|> T.pack <$> noFollowSpaceParens rawOp where
    noFollowSpaceParens p = (lexsym "(" *> p <* char ')')
uname = fmap T.pack $ (:) <$> Tpc.upper <*> Tp.many (P.identLetter style)

qualified p = singleton p <|> ((:) <$> lname <*> dot (qualified p)) where
    dot p   = char '.' *> p

nonReserved f p = p >>= \name -> 
    if T.unpack (f name) `elem` P.reservedNames style
        then Tp.unexpected ("reserved word " ++ show name)
        else return name

--
qualifiedName = P.lexeme lexer $ nonReserved head $ Tp.sepBy1 oplname (char '.')
qualifiedSymbol = P.lexeme lexer $ nonReserved head $ qualified symbol where
    symbol = T.cons <$> char ':' <*> lname

--
lexsym = P.symbol lexer
bool = (lexsym "true" *> pure True) <|> (lexsym "false" *> pure False)
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
binary  p assoc f = Infix (p >>= return . f) assoc

-- TODO use 'sepEndBy'
comma = P.comma lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer


-- parse primitive "" "true"
primitive
  :: Tp.ParsecT String u Identity Expr
primitive
     =  const UNIT              <$> lexsym "()"
    <|> BOOL                    <$> Tp.try bool
    <|> DOUBLE                  <$> Tp.try float
    <|> INT                     <$> int
    --
    <|> CHAR                    <$> (P.charLiteral lexer)
    <|> STRING                  <$> text
    <|> RAWSTRING               <$> rawString  -- TODO competes with application
    --
    <|> SYMBOL                  <$> symbol
    <|> VAR                     <$> qualifiedName  -- TODO maybe move to expr?


