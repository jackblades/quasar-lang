{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST where

--
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.IntMap     as IM
import           Data.Text       (Text)
import           Text.Parsec     as Tp

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
    | QList         (IntMap FParser)                                       -- l[1,2,3]
    | QTuple        (IntMap FParser)
    | QMap          (Map FParser FParser)                                     -- m{ 'a'=32 }
    | Record        (Map Label FParser)
    ----
    | Constructor   Name
    | Match         FParser                    [(FParser, FParser)]              -- TODO PATTERN
    | If            [(FParser, FParser)]
    | Lambda        [Label]                 [Name]          FParser
    ----
    | Apply         FParser                    FParser
    | Block         [Dosyntax FParser]
    | DoNotation    [Dosyntax FParser]
    | Throw         FParser
    | Exception     FParser                    [(FParser, FParser)]          (Maybe FParser)
    ----
    | QUOTE         FParser
    | SPLICE        FParser
    | Macro         [Label]                 [Name]          FParser
    --
    | ERROR         Text
    deriving (Show, Eq, Ord)


--
data Src a = Src 
    { _start :: Tp.SourcePos
    , _expr  :: a
    , _end   :: Tp.SourcePos
    } deriving (Show, Eq, Ord)

type FParser = Src Expr