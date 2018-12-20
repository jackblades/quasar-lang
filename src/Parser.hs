
module Parser where

-- TODO pPrint $ parse form "" "#\"ajitsingh\\c\""

--
import Prelude
import           AST
import           Lexer hiding (symbol)
import Text.Parsec hiding (choice)
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Debug.Trace (trace)
import Data.Functor.Identity (Identity)
import Control.Monad (mzero)
import Text.Pretty.Simple (pPrint)
import Text.Parsec.ByteString (parseFromFile)


noSrcOp = noSrc . QLiteral . QSymbol . T.pack
opAST op = \a b -> 
    spanSrc a b $ QForm [noSrcOp op, a, b]

--
form :: ParsecT String u Identity (TextExpr a)
form = buildExpressionParser optable terms1 where
    terms1  = src $ fmap QForm $ many1 term
    optable = [[ binary (lexsym "$") AssocRight $ opAST "$" ]]

term    = choice [ list, vector, qmap, reader_macro, literal ]

forms = src $ fmap QForm $ many form
cforms = sepEndBy forms comma
field = (,) <$> form <*> (colon *> forms)
cfields = sepEndBy field comma

list = src $ fmap QList $ parens cforms
vector = src $ fmap QVector $ brackets cforms
qmap = src $ fmap QMap $ braces cfields
set = src $ fmap QSet 
    $ between (lexsym "#{") (lexsym "}") cforms

reader_macro = choice
    [ lambda
    , meta_data
    , regex
    , var_quote
    , host_expr
    , set
    , tag
    , discard
    , dispatch
    , deref
    , quote
    , backtick
    , unquote
    , unquote_splicing
    , gensym
    ]
    
quote
    = src $ fmap QQuote $ string "\\" *> term

backtick
    = src $ fmap QBacktick $ string "`" *> term

unquote
    = src $ fmap QUnquote $ string "~" *> term

unquote_splicing
    = src $ fmap QUnquoteSplicing $ string "~@" *> term

tag
    = string "^" *> (src $ QTag <$> term <*> form)

deref
    = src $ fmap QDeref $ string "@" *> term

gensym
    = (src $ fmap (QLiteral . QSymbol) $ qsymbol) <* Lexer.char '#'

lambda
    -- : '#(' form* ')'
    = string "#" *> (src $ QLambda <$> brackets cforms <*> term) -- TODO

meta_data
    = string "#^" *> (src $ QMetadata <$> parseMaybe qmap <*> term)

var_quote
    = string "#\\" *> (src $ fmap QLiteral symbol)

host_expr
    = string "#+" *> (src $ QHostExpr <$> term <*> form)

discard
    = string "#_" *> (src $ fmap QDiscard $ term)

dispatch
    = string "#" *> (src $ QDispatch <$> (fmap symbolToText symbol) <*> form)

regex
    = string "#" *> (src $ fmap QLiteral $ qstring)

literal :: ParsecT String u Identity (TextExpr a)
literal
    = src $ fmap QLiteral $ choice
        [ qstring
        , qfloat
        , qint
        , qchar
        , nil
        , qbool
        , keyword
        , symbol
        , param_name
        ]

qstring = fmap QString text
qint = fmap (QInt . fromInteger) int
qfloat = fmap QFloat float 
-- qchar = Lexer.char '\\' *> fmap QChar anyChar
qchar = fmap QChar charLiteral 
nil = lexsym "nil" *> return QNil
qbool = fmap QBool bool

keyword = choice [ macro_keyword, simple_keyword ]
simple_keyword = Lexer.char ':' *> symbol
macro_keyword = string "::" *> symbol

symbol = lexeme 
    $ fmap QSymbol 
    $ choice [ ns_symbol, simple_sym ]
simple_sym = qsymbol
ns_symbol = do 
    ns <- identifier 
    Lexer.char '/' 
    sym <- qsymbol
    return $ mconcat [ns, T.pack "/", sym]

qsymbol = choice [ fmap T.singleton (oneOf "./"), ident ] -- TODO identifier eats spaces
param_name = fmap QParam . fmap T.pack $ 
    Lexer.char '%' *> (try num <|> lexsym "&")
    where num = (:) <$> oneOf "123456789" <*> many (oneOf "0123456789")


-- primitives
-- list, map, tuple, record, constructor
-- match, if
-- lambda
-- application
-- blocks
-- do notation
-- exceptions
-- quote, unquote
-- macros
-- infix operators, operators sections
-- ffi-code
-- 
-- typesystem, lens stuff
-- syntax-macro
-- INFIX optable from source
-- GENERALIZED ffi-lang

-- comprehensions -- useless given do_not?


--
choice ps = foldr (<|>) mzero $ fmap try ps
parseMaybe p = try (fmap Just p) <|> return Nothing
symbolToText (QSymbol s) = s
parseFile p fname
    = do input <- readFile fname
         let o = parse (whitespace *> p <* whitespace) fname input
         pPrint o