
module TextASTParser where
-- see https://github.com/Zankoku-Okuno/hexpr/blob/master/examples/etalisp/basic.el
-- and https://personal.cis.strath.ac.uk/conor.mcbride/pub/Frank/test.fk
--
import           Prelude
import           TextAST
import           TextASTLexer hiding (symbol)
import           Text.Parsec hiding (choice, char)
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Data.List (sort)
import           Control.Applicative (liftA2)
import           Data.Monoid ((<>))
import           Debug.Trace (trace, traceShow)
import           Data.Functor.Identity (Identity)
import           Control.Monad (mzero)
import           Text.Pretty.Simple (pPrint)
import           Text.Parsec.ByteString (parseFromFile)

noSrcOp = noSrc . QLiteral . QSymbol . T.pack
opAST op = \a b -> 
    spanSrc a b $ QForm [noSrcOp op, a, b]
opASTUnary op = \a -> 
    spanSrc a a $ QForm [noSrcOp op, a]
    
-- TODO Add Logging 
    -- Like each choice should report what point the parse failed
-- TODO ternary op for maybe / either
-- TODO if / let / case
-- TODO multimethods, conditions, deeplazy

--
topLvl = opAST "=" <$> forms <*> (equalP *> whereExp)      -- f x y = whereExpr
whereExp = do       -- expr where { a = fx, ... }
    lhs <- form
    parseMaybe (lexsym "where") >>= \case
        Nothing -> return lhs
        Just _  -> do
             let binding = (,) <$> forms <*> (equalP *> whereExp)
             rhs <- qMap $ braces (commaSep binding)
             return $ opAST "where" lhs rhs

-- expr starts here
form :: Parser (Src a)
form = buildInfixParser bin_optable forms       -- defines infix application

bin_optable = M.fromList $ descendingPrec ops where
    binaryOp = (,)
    descendingPrec ioptable = do
        (prec, ops) <- zip [100,99..0] ioptable
        (op, assoc) <- ops
        return (op, (assoc, prec)) 
    ops = [ -- (8)
          [ binaryOp "*" AssocLeft
          , binaryOp "/" AssocLeft
          , binaryOp "%" AssocLeft
          , binaryOp "." AssocLeft ]
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
      ]

forms  = qForm $ many1 term     -- defines prefix application (f a b ...)
term =
    choice (fmap parsePrefix pre_optable) <|> term2
  where
    parsePrefix op =
        opASTUnary <$> string op <*> term2
pre_optable = reverse . sort $
    [ "~@"
    , "~"
    , "!"
    -- , "\\"  -- conflicts with lambda
    , "`"
    , "@"
    , "#" 
    -- , "%" 
    ]

term2 = choice1 $ productDef <> prim where    -- defines the primitives
    prim = [ lambda, gensym, literal ]
    productDef =
        [ qprod "(|" cforms "|)" QIdiom
        , qprod "(" cforms ")" (\x -> if length x == 1 then _expr (head x) else QList x)
        , qprod "[" cforms "]" QVector
        , qprod "{|" qdo "|}" QDo
        , qprod "{" qmap "}" QMap
        , qprod "#{" cforms "}" QSet
        , qprod ":{" qraw "}" (QLiteral . QRaw . T.pack)
        ]
    --
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
    -- 
    cforms = commaSep form
    field = (,) <$> term <*> (colon *> form)
    cfields = commaSep field

--
lambda = src $ do
    lexsym "\\"
    args <- many1 identifier
    lexsym "->"
    body <- whereExp
    return $ QLambda args body

gensym
    = qGensym qsymbol <* lexsym "#"
--
literal :: Parser (Src a)
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
        ]

qstring = fmap QString text
qint = fmap (QInt . fromInteger) int
qfloat = fmap QFloat float 
-- qchar = TextASTLexer.char '\\' *> fmap QChar anyChar
qchar = fmap QChar charLiteral 
nil = lexsym "nil" *> return QNil
qbool = fmap QBool bool

keyword = choice1 [ macro_keyword, simple_keyword ]
simple_keyword = TextASTLexer.char ':' *> symbol
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
    TextASTLexer.char '/' 
    sym <- qsymbol
    return $ mconcat [ns, T.pack "/", sym]

qsymbol = ident -- identifier eats spaces


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
choice1 = choice
-- choice1 ps = case ps of
--     []   -> mzero
--     x:[] -> x
--     x:xs -> try x <|> choice1 xs
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
-- because buildExpressionParser doesn't handle similar operators well (like >> and >>=)
buildInfixParser optable term =  fmap shuntingYard parseInfixTermsList where
    itTerm = fmap TERM term
    itOperator = fmap OPERATOR operator
    operator = choice1 $ fmap lexsym $ reverse $ M.keys optable

    -- parses 'x (op y)*' into [x, op, y, op, y, ...]
    parseInfixTermsList = (:) <$> itTerm <*> parseOptionalTerms where
        parseOptionalTerms = try manyOpTerm <|> return []
        manyOpTerm = do
            op <- itOperator
            y  <- itTerm
            t' <- parseOptionalTerms
            return (op : y : t') 

    -- runs the shunting yard algorithm to parse the list into a tree
    shuntingYard xs = head . foldRemaining . foldList $ xs where
        foldList = foldl f ([], [])

        -- f (ts, os) b | traceShow (length ts, os, b) False = undefined  -- DEBUG
        f (terms,operators)        (TERM a)     = (a : terms, operators)
        f (terms,[])               (OPERATOR a) = (terms    , [a])
        f (x:y:terms, o:operators) (OPERATOR a) = 
            if | prec_A < prec_O  -> f reduceTermOnly (OPERATOR a)
               | prec_A > prec_O  -> shift
               | prec_A == prec_O -> case (assoc_O, assoc_A) of
                    (AssocLeft, _)         -> reduce
                    (AssocNone, AssocLeft) -> reduce
                    otherwise              -> shift
          where
            -- 'Nothing' case can't happen because it won't parse
            -- can happen when operators become user-definable
            Just (assoc_A, prec_A) = M.lookup a optable  
            Just (assoc_O, prec_O) = M.lookup o optable
            shift = (x:y:terms, a:o:operators)
            reduce = (opAST o y x : terms, a:operators)
            reduceTermOnly = (opAST o y x : terms, operators)
        
        foldRemaining (terms, operators) = foldl reduce1 terms operators where
            reduce1 (x:y:terms) o = opAST o y x : terms

-- EXPERIMENTAL
-- requires adding any reserved names to 'reservedNames' in the Lexer
buildMixfixParser optable term = 
    choice1 $ fmap parseMixfix optable where
        parseMixfix [] = error "mixfix optable contains an empty operator\n - maybe we can just ignore empty"
        parseMixfix xs = fmap (noSrc . QForm) 
                       -- $ sequence $ fmap (\x -> opASTUnary <$> x <*> term) xs
                       $ sequence [ opASTUnary <$> (lexsym "if")   <*> term
                                  , opASTUnary <$> (lexsym "then") <*> term
                                  , opASTUnary <$> (lexsym "else") <*> term ]

mixfix_optable =
    [ [lexsym "if", lexsym "then", lexsym "else"]
    ]



