
module Parser where

--
import           Control.Applicative     ((<|>))
import           AST
import           Lexer
import qualified Text.Parsec as Tp
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Debug.Trace (trace)
import Data.Functor.Identity (Identity)

-- TODO make sure that full words are parsed
-- TODO ex: Tp.parse expr "" "throwx" = Throw (VAR ["x"]) which is obviosly wrong
--      or make all reserved words invalid identifiers (like throw:)

operator = Tp.try infixNonOp <|> symOp where
    infixNonOp = char '.' *> (fparser (VAR <$> qualifiedName) <|> parens expr)
    symOp = fparser $ fmap VAR $ singleton $ T.pack <$> rawOp

opSection = parens (lsec <|> rsec) where
    lsec = lsecf <$> operator <*> term2
    rsec = rsecf <$> term2 <*> operator
    lsecf op y = spanSrc op y (Lambda [T.pack "x"] [] 
        $ spanSrc op y $ Apply op (spanSrc op y (QTuple (IM.fromList [(0, noSrc $ VAR [T.pack "x"]),(1,y)]))))
    rsecf x op = spanSrc op x (Lambda [T.pack "y"] [] 
        $ spanSrc op x $ Apply op (spanSrc op x (QTuple (IM.fromList [(0,x),(1, noSrc $ VAR [T.pack "y"])]))))

--
list = fparser $ QList . IM.fromList . zip [0..] <$> (brackets $ commaSep expr)

tuple = fparser $ QTuple . IM.fromList . zip [0..] 
    <$> (parens $ (:) <$> (expr <* comma) 
                      <*> (commaSep1 expr <|> pure []))
    -- TODO convert to record

mapL = fparser $ QMap . M.fromList <$> (braces $ commaSep $ assign) where
    assign = (,) 
         <$> expr 
         <*> (lexsym ":" *> expr)

record = fparser $ Record . M.fromList <$> (braces $ commaSep $ assign) where
    assign = (,) 
         <$> identifier
         <*> (lexsym "=" *> expr)
    -- TODO record accessors, update etc

--
constructor = fparser $ Constructor <$> qualified uname
match = fparser $ Match <$> (lexsym "//" *> expr) <*> cases
pattern = primitive  -- TODO change to actual patterns
cases = braces (semiSep1 cs) where
    cs = (,) <$> pattern <*> (arrow *> expr) where
        arrow = lexsym "=>"    

--
ifst = fparser $ If <$> (lexsym "if" *> braces conditions) where
    conditions = semiSep1 condition
    condition  = (,) <$> expr <*> (arrow *> expr) where
        arrow = lexsym "=>"        
    
ifte =  fparser $ (\ c t f -> If [(c, t), (elseSymbol, f)])
    <$> (lexsym "if"   *> expr) 
    <*> (lexsym "then" *> expr) 
    <*> (lexsym "else" *> expr)
    <*  lexsym ";" where
        elseSymbol = noSrc (SYMBOL $ T.pack "else")

lambda = fparser $ Lambda <$> (lexsym "\\" *> args) <*> effects <*> (arrow *> body) where
    args = commaSep1 identifier  -- TODO conver to record
    effects = (lexsym "|" *> commaSep (singleton identifier)) <|> pure [] 
    arrow = lexsym "->"
    body = expr

lambdaCase = fparser $ lexsym "\\\\"
     *> (Lambda [T.pack "x"] <$> effects <*> fparser (Match argx <$> cases)) where
    effects = (commaSep (singleton identifier)) <|> pure [] 
    argx = noSrc (VAR [T.pack "x"])

application = fparser $ RAWSTRING <$> rawString <|> Apply <$> name <*> args where
    name = Tp.try (fparser $ VAR <$> qualifiedName) <|> Tp.try constructor <|> Tp.try opSection <|> parens expr
    args = do 
        arg1 <- term2
        arg2 <- Tp.try term2 <|> pure (noSrc $ ERROR $ T.pack "")
        return $ case _expr arg2 of
            ERROR _ -> arg1
            _       -> spanSrc arg1 arg2 $ QTuple (IM.fromList $ zip [0..] [arg1, arg2])

block = fparser $ braces (Block <$> semiSep st) where
    st = Tp.try assign <|> exp where
        assign = Assign  <$> identifier <*> (lexsym "="  *> expr)
        exp    = MExpr   <$> expr

do_not = fparser $ char '+' *> braces (DoNotation <$> semiSep1 st) where
    st = Tp.try assign <|> Tp.try effect <|> exp where
        assign = Assign  <$> identifier <*> (lexsym "="  *> expr)
        effect = MAssign <$> identifier <*> (lexsym "<-" *> expr)
        exp    = MExpr   <$> expr

throw = fparser $ Throw <$> (lexsym "throw:" *> expr)
exn = fparser $ Exception <$> (lexsym "t:" *> expr)
                <*> (lexsym "c:" *> cases)
                <*> (lexsym "f:" *> fmap Just expr 
                                <|> pure Nothing)

--
quote = fparser $ lexsym "%" *> (QUOTE <$> expr)
splice = fparser $ lexsym "$" *> (SPLICE <$> expr)
defmacro = fparser $ Macro <$> (lexsym "\\%" *> args) <*> effects <*> (arrow *> body) where
    args = commaSep1 identifier  -- TODO conver to record
    effects = (lexsym "|" *> commaSep (singleton identifier)) <|> pure [] 
    arrow = lexsym "->"
    body = expr

-- TODO generalized ffi not just java
ffiJava = lexsym "java" *> (braces $ Tp.many1 $ escapedEndBrace <|> others) where
    escapedEndBrace = Tp.try (lexsym "\\}" *> pure '}')
    others = Tp.noneOf "}"

--
reservedNamesExpr = match 
    <|> Tp.try ifst         <|> ifte 
    <|> Tp.try defmacro     <|> Tp.try lambdaCase   <|> lambda 
    <|> Tp.try exn          <|> throw               <|> Tp.try do_not
    <|> quote               <|> splice

compoundExpr = list         <|> tuple 
    <|> Tp.try mapL         <|> Tp.try record       <|> block

-- term2 :: Tp.ParsecT String u Identity FParser
term2 = Tp.try reservedNamesExpr
    <|> Tp.try primitive
    <|> Tp.try compoundExpr
    <|> constructor 
    <|> Tp.try opSection
    <|> parens expr

term  = Tp.try application <|> term2

-- TODO generate this from source
-- optable :: [[Text.Parsec.Expr.Operator String u Data.Functor.Identity.Identity Src]]
optable = [[binary operator AssocLeft
    (\op x y -> spanSrc x y (Apply op $ spanSrc x y (QTuple $ IM.fromList [(0,x), (1,y)])))]]

expr = buildExpressionParser optable term
   -- <?> "expression"

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