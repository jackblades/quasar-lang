
module Parser where

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

form :: ParsecT String u Identity (TextExpr a)
form = choice [ list, vector, qmap, reader_macro, literal ]

forms = src $ fmap QForm $ many form
cforms = sepEndBy forms comma
field = (,) <$> form <*> (colon *> forms)
cfields = sepEndBy field comma

list = src $ fmap QList $ parens cforms
vector = src $ fmap QVector $ brackets cforms
qmap = src $ fmap QMap $ braces cfields
set = src $ fmap QSet $ between (lexsym "#{") (lexsym "}") cforms

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
    = src $ fmap QQuote $ lexsym "\'" *> form

backtick
    = src $ fmap QBacktick $ lexsym "`" *> form

unquote
    = src $ fmap QUnquote $ lexsym "~" *> form

unquote_splicing
    = src $ fmap QUnquoteSplicing $ lexsym "~@" *> form

tag
    = lexsym "^" *> (src $ QTag <$> form <*> form)

deref
    = src $ fmap QDeref $ lexsym "@" *> form

gensym
    = (src $ fmap QLiteral $ symbol) <* lexsym "#"

lambda
    -- : '#(' form* ')'
    = lexsym "#" *> (src $ QLambda <$> brackets cforms <*> form) -- TODO

meta_data
    = lexsym "#^" *> (src $ QMetadata <$> parseMaybe qmap <*> form)

var_quote
    = lexsym "#\'" *> (src $ fmap QLiteral symbol)

host_expr
    = lexsym "#+" *> (src $ QHostExpr <$> form <*> form)

discard
    = lexsym "#_" *> (src $ fmap QDiscard $ form)

dispatch
    = lexsym "#" *> (src $ QDispatch <$> (fmap symbolToText symbol) <*> form)

regex
    = lexsym "#" *> (src $ fmap QLiteral $ qstring)

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
qchar = fmap QChar charLiteral
nil = lexsym "nil" *> return QNil
qbool = fmap QBool bool

keyword = choice [ macro_keyword, simple_keyword ]
simple_keyword = Lexer.char ':' *> symbol
macro_keyword = lexsym "::" *> symbol

symbol = fmap QSymbol $ choice [ ns_symbol, simple_sym ]
simple_sym = qsymbol
ns_symbol = do ns <- identifier 
               Lexer.char '/' 
               sym <- qsymbol
               return $ mconcat [ns, T.pack "/", sym]

qsymbol = choice [ fmap T.singleton (oneOf "./"), identifier ]
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