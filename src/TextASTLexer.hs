-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module TextASTLexer where

--
import           Prelude
import           Control.Applicative     ((<|>))
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Char as Tpc
import qualified Text.Parsec as Tp
import qualified Data.Text as T
import           Text.Parsec.Expr (Operator(..), Assoc(..))
import           Data.Functor.Identity (Identity)

import           TextAST 
import           Debug.Trace (trace)
import           Control.Monad (mzero)


nums = "0123456789"
dotnums = ".0123456789"
langwords = "^`\'\"#~@:/%()[]{}$"  -- '\\'
spcs = " \n\r\t,"
opLetter = ":!#$%&*+./<=>?@\\^|-~"

lexer = P.makeTokenParser style
style :: P.LanguageDef st
style = emptyDef
    { P.commentLine    = "--"
    , P.identStart     = Tpc.letter <|> Tpc.oneOf "_:"
    , P.identLetter    = Tpc.alphaNum
                     <|> Tp.try (Tpc.oneOf "_/:.-" <* Tp.notFollowedBy whitespace)
    , P.caseSensitive  = True
    , P.opStart        = Tpc.oneOf opLetter
    , P.opLetter       = Tpc.oneOf opLetter
    , P.reservedOpNames= []
    , P.reservedNames  = ["where"]
    --
    , P.commentStart   = ""
    , P.commentEnd     = ""
    , P.nestedComments = False
    }

--
singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

nonReserved p = do
    name <- p
    if name `elem` P.reservedNames style
        then mzero -- Tp.unexpected ("reserved word " ++ show name)
        else return (T.pack name)

ident = nonReserved $ do
    c <- P.identStart style
    cs <- Tp.many (P.identLetter style)
    return (c:cs)

--
lexeme = P.lexeme lexer
lexsym = P.symbol lexer
bool = (lexsym "true" *> pure True) 
   <|> (lexsym "false" *> pure False)

whitespace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
-- natural = fmap read (Tp.many1 Tp.digit) <* lexsym "i"
natural = P.natural lexer
unsignedFloat = do
    hd <- Tp.many1 Tp.digit
    tl <- Tp.try ((:) <$> char '.' <*> Tp.many1 Tp.digit) <|> return ""
    lexsym "f"
    return $ readf (hd ++ tl )
    where readf = read :: [Char] -> Double

int = natural <|> (char '-' *> fmap negate natural)
float = unsignedFloat <|> (char '-' *> fmap negate unsignedFloat)
char = Tpc.char
text = fmap T.pack (P.stringLiteral lexer)
identifier = fmap T.pack (P.identifier lexer)
rawString = char 'r' *> text
symbol = T.cons <$> char ':' <*> identifier

rawOp = P.operator lexer
binary  p assoc f = Infix (p >> return f) assoc
prefix  p       f = Prefix (p >> return f)
postfix p       f = Postfix (p >> return f)

--
equalP = lexsym "="
comma = P.comma lexer
commaSep p = Tp.sepEndBy p comma
commaSep1 p = Tp.sepEndBy1 p comma
colon = P.colon lexer
colonSep p = Tp.sepEndBy p colon
colonSep1 p = Tp.sepEndBy1 p colon
semi = P.semi lexer
semiSep p = Tp.sepEndBy p semi
semiSep1 p = Tp.sepEndBy1 p semi
parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer