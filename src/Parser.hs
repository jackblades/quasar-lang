-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parser where

--
import           Control.Applicative     ((<|>))
import           AST
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Char as Tpc
import qualified Text.Parsec as Tp
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import           Data.Functor.Identity (Identity)

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Debug.Trace (trace)

-- TODO make sure that full words are parsed
-- TODO ex: Tp.parse expr "" "throwx" = Throw (VAR ["x"]) which is obviosly wrong
--      or make all reserved words invalid identifiers (like throw:)

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
    , P.opLetter       = Tpc.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedOpNames= ["\\", "//", "\\\\", "->", "=>", "<-", "%", "$"] -- :
    , P.reservedNames  = ["true", "false", "if", "then", "else", "match", "do", "try", "catch", "finally", "throw"]
    , P.caseSensitive  = True
    }

singleton :: Functor f => f a -> f [a]
singleton = fmap (:[])

lname = fmap T.pack $ (:) <$> P.identStart style <*> Tp.many (P.identLetter style)
uname = fmap T.pack $ (:) <$> Tpc.upper <*> Tp.many (P.identLetter style)

qualified p = singleton p <|> ((:) <$> lname <*> dot (qualified p)) where
    dot p   = char '.' *> p

nonReserved f p = p >>= \name -> 
    if T.unpack (f name) `elem` P.reservedNames style
        then Tp.unexpected ("reserved word " ++ show name)
        else return name

--
qualifiedName = P.lexeme lexer $ nonReserved head $ Tp.sepBy1 lname (char '.')
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

rawOp = fmap VAR $ singleton $ T.pack <$> P.operator lexer
operator = Tp.try (char '.' *> (VAR <$>qualifiedName <|> parens expr)) <|> rawOp where
    backQuote = (lexsym "`")
    between p = Tp.between p p

reservedOp = P.reservedOp lexer
binary  p assoc f = Infix (p >>= return . f) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

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
    <|> RAWSTRING               <$> rawString
    --
    <|> SYMBOL                  <$> symbol
    <|> VAR                     <$> qualifiedName  -- TODO maybe move to expr?

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
     *> (Lambda [T.pack "x"] <$> effects  -- TODO fix names issue
    <*> (Match (VAR [T.pack "x"]) <$> cases)) where
    effects = (commaSep (singleton identifier)) <|> pure [] 

application = Apply <$> name <*> args where
    name = Tp.try (VAR <$> qualifiedName) <|> Tp.try constructor <|> parens expr
    args = do 
        arg1 <- term2
        arg2 <- Tp.try term2 <|> pure UNIT
        return $ case arg2 of
            UNIT -> arg1
            _    -> QTuple $ IM.fromList $ zip [0..] [arg1, arg2]

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
exn = Exception <$> (lexsym "t:"     *> expr)
                <*> (lexsym "c:"   *> cases)
                <*> (lexsym "f:" *> fmap Just expr 
                                     <|> pure Nothing)

--
quote = lexsym "%" *> (QUOTE <$> expr)
splice = lexsym "$" *> (SPLICE <$> expr)

--
reservedNamesExpr = match 
    <|> Tp.try ifst         <|> ifte 
    <|> Tp.try lambdaCase   <|> lambda 
    <|> Tp.try exn          <|> throw               <|> Tp.try do_not
    <|> quote               <|> splice

compundExpr = list          <|> tuple 
    <|> Tp.try mapL         <|> Tp.try record

term2 = Tp.try reservedNamesExpr
    <|> Tp.try primitive
    <|> Tp.try compundExpr
    <|> block
    <|> constructor 
    <|> parens expr

term  = Tp.try application <|> term2

optable = [[binary operator AssocLeft
    (\op x y -> Apply op . QTuple $ IM.fromList [(0,x), (1,y)])]]

expr = buildExpressionParser optable term
   -- <?> "expression"




-- testing
assign2 = (,) 
    <$> identifier
    <*> (lexsym "=" *> expr)
parseFromFile p fname
     = do input <- readFile fname
          return $ Tp.parse (P.whiteSpace lexer *> p) fname input

-- primitives
-- list, map, tuple, record, constructor
-- match, if
-- lambda
-- application
-- blocks
-- do notation
-- exceptions
-- quote, unquote
-- 
-- typesystem, lens stuff
-- macros
-- infix operators, sections
-- syntax-macro
-- ffi-code

-- comprehensions -- useless given do_not?