{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Parser where

--
import           Prelude
import           AST
import           Lexer hiding (symbol, identifier)
import           Text.Parsec hiding (choice)
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Data.Monoid ((<>))
import           Debug.Trace (trace, traceShow)
import           Data.Functor.Identity (Identity)
import           Control.Monad (mzero)
import           Text.Pretty.Simple (pPrint)
import           Text.Parsec.ByteString (parseFromFile)

-- TODO type annotations
-- TODO syntax macros

noSrcOp = noSrc . QLiteral . QSymbol . T.pack
opAST op = \a b -> 
    spanSrc a b $ QForm [noSrcOp op, a, b]
opASTUnary op = \a -> 
    spanSrc a a $ QForm [noSrcOp op, a]
    

--
topLvl = opAST "=" <$> forms <*> (equalP *> whereExp)      -- f x y = whereExpr
whereExp = do       -- expr where { a = fx, ... }
    lhs <- form
    try $ do lexsym "where"
             let binding = (,) <$> forms <*> (equalP *> whereExp)
             rhs <- src $ fmap QMap $ braces (sepEndBy binding comma)
             return $ opAST "where" lhs rhs
        <|> return lhs

-- expr starts here
form :: ParsecT String u Identity (TextExpr a)
form = buildInfixParser optable forms       -- defines infix application

optable = ops where
    binaryOp = (,)
    descendingPrec ioptable = do
        (prec, ops) <- zip [100,99..0] ioptable
        (op, assoc) <- ops
        return (op, (assoc, prec)) 
    ops = M.fromList $ descendingPrec [ -- (8)
        [ binaryOp "*" AssocLeft
        , binaryOp "/" AssocLeft
        , binaryOp "%" AssocLeft   -- TODO conflicts with param_name
        , binaryOp "." AssocLeft]  -- TODO parsed as QSymbol
        -- (6)
        , [ binaryOp "+" AssocLeft
        , binaryOp "-" AssocLeft ]
        -- (6)
        , [ binaryOp ">>" AssocLeft
        , binaryOp "<<" AssocLeft ]
        -- logical operators (4)  
        , [ binaryOp ">" AssocNone
        , binaryOp "<" AssocNone
        , binaryOp ">=" AssocNone
        , binaryOp "<=" AssocNone ]
        --
        , [ binaryOp "==" AssocNone
        , binaryOp "!=" AssocNone ]
        -- logical operators (3)
        , [ binaryOp "&" AssocLeft ]
        , [ binaryOp "^" AssocLeft ]
        , [ binaryOp "|" AssocLeft ]
        , [ binaryOp "&&" AssocLeft ]
        , [ binaryOp "||" AssocLeft ]
        --
        , [ binaryOp "+=" AssocRight
        , binaryOp "-=" AssocRight
        , binaryOp "*=" AssocRight
        , binaryOp "/=" AssocRight
        , binaryOp "%=" AssocRight
        , binaryOp "<<=" AssocRight
        , binaryOp ">>=" AssocRight
        , binaryOp "&=" AssocRight
        , binaryOp "|=" AssocRight
        , binaryOp "^=" AssocRight ]
        -- general operators
        , [ binaryOp "$" AssocRight ]
        , [ binaryOp "::" AssocRight ]
        -- handled at a higher level
        --   , [ binary (lexsym "where") AssocRight $ opAST "where" ]
        --   , [ binary (lexsym "=") AssocRight $ opAST "="]
      ]

forms  = src $ fmap QForm $ many1 term     -- defines prefix application (f a b ...)
term = buildExpressionParser optable term2 where
    prefixOp op = prefix (try $ string op) $ opASTUnary op
    optable = 
        [ [ prefixOp "~@"  -- TODO conflict betweem '~' and '~@'
          , prefixOp "~"
          , prefixOp "!"
          , prefixOp "\\"
          , prefixOp "`"
          , prefixOp "@"
          , prefixOp "#" ]
        ]

-- ("(", ")", [(",", QList)], f)


term2 = choice1 $ productDef <> prim where    -- defines the primitives
    prim = [ reader_macro, literal ]
    productDef =
        [ qprod "(|" cforms "|)" QIdiom
        , qprod "(" cforms "|)" QList
        , qprod "[" cforms "]" QVector
        , qprod "{|" qdo "|}" QDo
        , qprod "{" qmap "}" QMap
        , qprod "#{" cforms "}" QSet
        , qprod ":{" qraw "}" (QLiteral . QRaw . T.pack)
        ]

    qdo = semiSep $ choice1 [assign, bind, form] where
        assign = opAST "=" <$> forms <*> (equalP *> whereExp)
        bind   = opAST "<-" <$> forms <*> (lexsym "<-" *> whereExp)

    qmap = try cfields <|> withIndex cforms where
        withIndex = fmap index
        index = zip (fmap indexConstructor [0 .. ])
        indexConstructor = noSrc . QLiteral . QInt

    qraw = many1 $ escapedEndBrace <|> others where
        escapedEndBrace = try (lexsym "\\}" *> pure '}')
        others = noneOf "}"

    qprod beg p end f = lexsym beg *> src (fmap f p) <* lexsym end

-- forms = src $ fmap QForm $ many form
cforms = sepEndBy form comma
field = (,) <$> term <*> (colon *> form)
cfields = sepEndBy field comma

--
ternary = do  -- TODO
    e <- forms
    lexsym "?"
    t <- form
    lexsym ":"
    f <- form
    return $ spanSrc e f $ QForm [noSrcOp "?:", e, t, f]

--
reader_macro = choice1
    [ regex
    , lambda
    , gensym
    ]

lambda
    -- : '#(' form* ')'
    = string "#" *> src (QLambda <$> args <*> term)
    where args = try (brackets cforms) <|> pure []

regex
    = string "#" *> src (fmap (QLiteral . QRegex) text)

gensym
    = (src $ fmap (QLiteral . QGensym) $ qsymbol) <* lexsym "#"
--
literal :: ParsecT String u Identity (TextExpr a)
literal
    = src 
    $ fmap QLiteral 
    $ choice1
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

keyword = choice1 [ macro_keyword, simple_keyword ]
simple_keyword = Lexer.char ':' *> symbol
macro_keyword = string "::" *> symbol

symbol = lexeme 
    $ fmap QSymbol 
    $ do sym <- choice1 [ ns_symbol, simple_sym ]
         if sym == T.pack ":"
            || sym == T.pack "::"
         then mzero
         else return sym
simple_sym = qsymbol
ns_symbol = do 
    ns <- ident 
    Lexer.char '/' 
    sym <- qsymbol
    return $ mconcat [ns, T.pack "/", sym]

qsymbol = choice1 [ fmap T.singleton (oneOf "."), ident ] -- identifier eats spaces
param_name = fmap QParam . fmap T.pack $ 
    Lexer.char '%' *> (try num <|> lexsym "&")  -- TODO (LAST PART IS OPTIONAL)
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
choice1 ps = case ps of
    []   -> mzero
    x:[] -> x
    x:xs -> try x <|> choice1 xs
parseMaybe p = try (fmap Just p) <|> return Nothing
symbolToText (QSymbol s) = s
parseFile p fname
    = do input <- readFile fname
         let o = parse (whitespace *> p) fname input
         pPrint o

--
data IdentType a b
    = TERM a
    | OPERATOR b
    -- deriving (Show)
instance Show b => Show (IdentType a b) where
    show (TERM _) = "TERM"
    show (OPERATOR op) = "OPERATOR " <> show op
instance Show Assoc where
    show AssocLeft = "AssocLeft"
    show AssocNone = "AssocNone"
    show AssocRight = "AssocRight"

-- buildInfixParser :: b -> TextExpr a -> ParsecT String u Identity (TextExpr a)
buildInfixParser optable term =  fmap listToTree parseList where
    itTerm = fmap TERM term
    itOperator = fmap OPERATOR operator
    operator = choice1 $ fmap (try . lexsym) $ reverse $ M.keys optable

    -- parses 'x (op y)*' into [x, op, y, op, y, ...]
    parseList = (:) <$> itTerm <*> go where
        go = do
            op <- itOperator
            y <- itTerm
            t' <- try go <|> return []
            return (op : y : t') 

    -- runs the shunting yard algorithm to parse the list into a tree
    listToTree xs = head . foldRemaining . foldList $ xs where
        foldList = foldl f ([], [])

        -- f (ts, os) b | traceShow (length ts, os, b) False = undefined  -- DEBUG
        f (terms,operators)  (TERM a)     = (a : terms, operators)
        f (terms,[])         (OPERATOR a) = (terms    , [a])
        f (x:y:terms, o:operators) (OPERATOR a) = 
            if | prec_A < prec_O  -> f reduceTermOnly (OPERATOR a)  -- TODO modify reduce?
               | prec_A > prec_O  -> shift
               | prec_A == prec_O -> case (assoc_O, assoc_A) of
                    (AssocLeft, _)         -> reduce
                    (AssocNone, AssocLeft) -> reduce
                    otherwise              -> shift
          where
            Just (assoc_A, prec_A) = M.lookup a optable  -- TODO handle failure
            Just (assoc_O, prec_O) = M.lookup o optable  -- TODO handle failure
            shift = (x:y:terms, a:o:operators)
            reduce = (opAST o x y : terms, a:operators)
            reduceTermOnly = (opAST o x y : terms, operators)
        

    foldRemaining (terms, operators) = foldl reduce1 terms operators where
        reduce1 (x:y:terms) o = opAST o x y : terms

