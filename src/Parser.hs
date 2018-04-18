
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

-- TODO make sure that full words are parsed
-- TODO ex: Tp.parse expr "" "throwx" = Throw (VAR ["x"]) which is obviosly wrong
--      or make all reserved words invalid identifiers (like throw:)

operator = Tp.try infixNonOp <|> symOp where
    infixNonOp = char '.' *> (VAR <$> qualifiedName <|> parens expr)
    symOp = fmap VAR $ singleton $ T.pack <$> rawOp

opSection = parens (lsec <|> rsec) where
    lsec = lsecf <$> operator <*> term2
    rsec = rsecf <$> term2 <*> operator
    lsecf op y = Lambda [T.pack "x"] [] (Apply op (QTuple (IM.fromList [(0,VAR [T.pack "x"]),(1,y)])))
    rsecf x op = Lambda [T.pack "y"] [] (Apply op (QTuple (IM.fromList [(0,x),(1,VAR [T.pack "y"])])))

--
list = QList . IM.fromList . zip [0..] <$> (brackets $ commaSep expr) where

tuple = QTuple . IM.fromList . zip [0..] 
    <$> (parens $ (:) <$> (expr <* comma) 
                      <*> (commaSep1 expr <|> pure []))
    -- TODO convert to record

mapL = QMap . M.fromList <$> (braces $ commaSep $ assign) where
    assign = (,) 
         <$> expr 
         <*> (lexsym ":" *> expr)

record = Record . M.fromList <$> (braces $ commaSep $ assign) where
    assign = (,) 
         <$> identifier
         <*> (lexsym "=" *> expr)
    -- TODO record accessors, update etc

--
constructor = Constructor <$> qualified uname
match = Match <$> (lexsym "//" *> expr) <*> cases
pattern = primitive  -- TODO change to actual patterns
cases = braces $ semiSep1 cs where
    cs = (,) <$> pattern <*> (arrow *> expr) where
        arrow = lexsym "=>"    

--
ifst = If <$> (lexsym "if" *> braces conditions) where
    conditions = semiSep1 condition
    condition  = (,) <$> expr <*> (arrow *> expr) where
        arrow = lexsym "=>"        
    
ifte =  (\ c t f -> If [(c, t), (SYMBOL $ T.pack "else", f)])
    <$> (lexsym "if"   *> expr) 
    <*> (lexsym "then" *> expr) 
    <*> (lexsym "else" *> expr)
    <*  lexsym ";"

lambda = Lambda <$> (lexsym "\\" *> args) <*> effects <*> (arrow *> body) where
    args = commaSep1 identifier  -- TODO conver to record
    effects = (lexsym "|" *> commaSep (singleton identifier)) <|> pure [] 
    arrow = lexsym "->"
    body = expr

lambdaCase = lexsym "\\\\"
     *> (Lambda [T.pack "x"] <$> effects
    <*> (Match (VAR [T.pack "x"]) <$> cases)) where
    effects = (commaSep (singleton identifier)) <|> pure [] 

application = Apply <$> name <*> args where
    name = Tp.try (VAR <$> qualifiedName) <|> Tp.try constructor <|> Tp.try opSection <|> parens expr
    args = do 
        arg1 <- term2
        arg2 <- Tp.try term2 <|> pure ERROR
        return $ case arg2 of
            ERROR -> arg1
            _     -> QTuple $ IM.fromList $ zip [0..] [arg1, arg2]

block = braces $ Block <$> semiSep st where
    st = Tp.try assign <|> exp where
        assign = Assign  <$> identifier <*> (lexsym "="  *> expr)
        exp    = MExpr   <$> expr

do_not = char '+' *> braces (DoNotation <$> semiSep1 st) where
    st = Tp.try assign <|> Tp.try effect <|> exp where
        assign = Assign  <$> identifier <*> (lexsym "="  *> expr)
        effect = MAssign <$> identifier <*> (lexsym "<-" *> expr)
        exp    = MExpr   <$> expr

throw = Throw <$> (lexsym "throw:" *> expr)
exn = Exception <$> (lexsym "t:" *> expr)
                <*> (lexsym "c:" *> cases)
                <*> (lexsym "f:" *> fmap Just expr 
                                <|> pure Nothing)

--
quote = lexsym "%" *> (QUOTE <$> expr)
splice = lexsym "$" *> (SPLICE <$> expr)
defmacro = Macro <$> (lexsym "\\%" *> args) <*> effects <*> (arrow *> body) where
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

term2 = Tp.try reservedNamesExpr
    <|> Tp.try primitive
    <|> Tp.try compoundExpr
    <|> constructor 
    <|> Tp.try opSection
    <|> parens expr

term  = Tp.try application <|> term2

-- TODO generate this from source
optable = [[binary operator AssocLeft
    (\op x y -> Apply op . QTuple $ IM.fromList [(0,x), (1,y)])]]

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