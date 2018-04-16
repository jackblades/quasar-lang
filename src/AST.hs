{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST where

--
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.IntMap     as IM
import           Data.Text       (Text)

type Label = Text
type Name = [Text]
newtype Operator = Operator Text
data Pattern
data Language  -- parse :: Parser Ast, build :: ParserT IO QuasarAST

data AppArgs n a
    = OneArg  a
    | TwoArg  a a
    | ManyArgs (Map n a)
    deriving (Show)

data OneOrBoth a b
    = OneL   a
    | OneR   b
    | BothLR a b
    deriving (Show)

data Dosyntax a
    = Assign        Label                 a                           -- x  = a
    | MAssign       Label                 a                           -- x <- a
    | MExpr         a                                                   -- a
    deriving (Show, Eq, Ord)


data Expr
    = UNIT
    | BOOL          Bool
    | INT           Integer
    | DOUBLE        Double
    --
    | CHAR          Char                                                -- 'c'
    | STRING        Text                                                -- "string"             unicode
    | RAWSTRING     Text                                                -- r"xx" r'xx' br'xx'   only escape \" or \'
    --
    | VAR           Name
    | SYMBOL        Text                                                -- :sym
    ----
    | QList         (IntMap Expr)                                       -- l[1,2,3]
    | QTuple        (IntMap Expr)
    | QMap          (Map Expr Expr)                                     -- m{ 'a'=32 }
    | Record        (Map Label Expr)
    ----
    | Constructor   Name
    | Match         Expr                    [(Expr, Expr)]              -- TODO PATTERN
    | If            [(Expr, Expr)]
    | Lambda        [Label]                 [Name]          Expr
    ----
    | Apply         Expr                    Expr
    | Block         [Dosyntax Expr]
    | DoNotation    [Dosyntax Expr]
    | Throw         Expr
    | Exception     Expr                    [(Expr, Expr)]          (Maybe Expr)
    ----
    | QUOTE         Expr
    | SPLICE        Expr
    deriving (Show, Eq, Ord)

mapINT f (INT x) = INT (f x)
