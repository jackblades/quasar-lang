{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST_OLD where

--
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Text       (Text)

newtype Label = Label Text
newtype Argname = Argname Text
newtype Operator = Operator Text
data Pattern
data Language  -- parse :: Parser Ast, build :: ParserT IO QuasarAST

data Primitive
    = BOOL          Bool
    | INT           Integer
    | DOUBLE        Double
    --
    | CHAR          Char                                                -- 'c'
    | STRING        Text                                                -- "string"             unicode
    | BYTESTRING    ByteString                                          -- b"string"            bytestring
    | RAWSTRING     Text                                                -- r"xx" r'xx' br'xx'   only escape \" or \'
    | RAWBYTESTRING Text
    --
    | SYMBOL        Text                                                -- :sym
    deriving (Show)

--
data AppArgs n a
    = OneArg  a
    | TwoArg  a a
    | ManyArgs (Map n a)

data OneOrBoth a b
    = OneL   a
    | OneR   b
    | BothLR a b

data Complex a
    = Var           Argname                 a
    | MultiIf       [(a, a)]                                            -- if  condition -> a
    | Record        (Map Label a)           a                           -- { x=2, y | a } => y=y
    | Case a        [(Pattern, a)]                                      -- x | pattern   -> a; _ -> a
    | Lambda        [Argname]               a                           -- \x : Int, y -> x + y  (single product arg)
    | App           a                       (AppArgs Argname a)         -- f x; f x y; f x=1, y=2, z=3; macro application
    | Try           a                       (OneOrBoth [a] a)           -- try (OneOrBoth [catch] finally)
    --
    | QMap          (Map a a)                                           -- m{ 'a'=32 }
    | QList         [a]                                                 -- l[1,2,3]
    --
    | QUOTE         a                                                   -- %(expr) = AST of expr
    | SPLICE        a                                                   -- $(EXPR) = value of expr (inside a macro)
    | Defmacro      [Argname]               a                           -- x = macro x y -> a; x :: AST -> AST -> AST

-- sugar and stuff
data Dosyntax a
    = Assign        Argname                 a                           -- x  = a
    | MAssign       Argname                 a                           -- x <- a
    | MExpr         a                                                   -- a

data OpSectionType
    = LeftSection
    | RightSection

data Fixity
    = InfixL Int
    | InfixR Int

data Syntax a t
    = FixityDecl    Text                    Fixity                      -- haskell like infixl and infixr
    | InfixApp      a                       a           a               -- x + y; x .then y; x .(f t) y
    | OpSection     OpSectionType           Operator    a               -- (op a) x = x op a; a op x = (a op x)
    | LambdaCase    [(Pattern, a)]                                      -- \case pattern -> a
    | IfThenElse    a                       a           a               -- if a then a else a
    | Idiom         a                                                   -- [: f a b :] maybe
    | Where         a                       a                           -- f = a where { x = ... }; x should be accessible
    | Namespace     [a]                                                 -- ns a.b.x; namespaces define scope
    --
    | Typed         a                       t                           -- a : Type
    --
    | Do            [Dosyntax a]                                        -- do monadicExpr
    | MonadComp     a                       [a]                         -- [x+y | x <- xs, y <- ys, x < y]
    --
    | SyntaxMacro   [Argname]               a                           -- x = syntaxmacro ...; x :: Syntax -> Syntax
    | CODE          Language                Text                        -- java #{ System.out.println("hello world"); }

--
newtype TParam = TParam Text
newtype TCon = TCon Text
newtype TArg = TArg Text

data TPrimitive
    = TBOOL
    | TINT
    | TFLOAT
    | TDOUBLE
    --
    | TCHAR
    | TSTRING
    | TBYTESTRING

data TProd t
    = TProd { _fields :: Map Argname t, _rest :: Maybe TArg }

data TSum t
    = TSum  { _products :: Map TCon t, _rest :: Maybe TCon }

newtype SOP t
    = SOP { _sop :: Either (TSum (SOP t)) (TProd (SOP t))}

data TDecl t
    = Newtype  { _tName :: Text, _tParams :: [TParam], _tExpr :: t }    -- newtype T = Text; newtype X a b = { a, b, ... }
    | Alias    { _tName :: Text, _tParams :: [TParam], _tExpr :: t }
    | Datatype { _tName :: Text, _tParams :: [TParam], _tExpr :: t }    -- can't have row variables in sum??

type Type t = SOP t

-- exposed compiler api
class Monad m => Compiler m a where
    warnc :: Text -> m a
    errorc :: Text -> m a
    assertc :: Bool -> m a
    typecheck :: Syntax (Type a) a -> m (Type a)  -- random signature just to compile, only the name is relevant
    macroexpand1 :: m a


-- ifM, do-rec
-- are record / local-namespace fields defined lazily? Should fields be recursive?

-- lazy, deeplazy
-- statemachines
-- typesystem constraints, typeclasses, forall etc
-- relational tables and sql
-- typed literal expression
-- FFI (C, java, haskell)

-- Partial class
-- lenses
-- effects
-- free monads

-- no globals - constructors, record fields, etc
-- record based modules, open in scope
-- typeclasses need to be global
-- Free should be efficient

{-
Non global Constructors
    data E a b = L a | R b
    x = E.L 32   : forall b. E Int b                        -- scoped constructors; works well with open sums

Extensible Product, Open Sums
    - Field : Record :: Case : Sum
        - f : {a , b |: r}           -> {c   |: r}        -- propagate fields information
        - f : (A | B |: R)           -> (C c |: R)        -- propagate case   information

Namespace vs Record
    - namespaces can contain data declarations and other top-level stuff (for compilation)
    - records can only contain fields data (for program logic)

-}
