module AST where

--
import           Data.ByteString (ByteString)
import           Data.Text       (Text)

data Label
data Argname
data Operator
data Pattern
data Language  -- parse :: Parser Ast, build :: ParserT IO QuasarAST

data Primitive
    = BOOL          Bool
    | INT           Int
    | FLOAT         Float
    | DOUBLE        Double
    --
    | CHAR          Char                                                -- 'c'
    | STRING        Text                                                -- "string"             unicode
    | BYTESTRING    ByteString                                          -- b"string"            bytestring
    | RAWSTRING     (Either ByteString Text)                            -- r"xx" r'xx' br'xx'   only escape \" or \'
    --
    | SYMBOL        Text                                                -- :sym
    | CODE          Language              Text                          -- java #{ System.out.println("hello world"); }

data AppArgs a
    = One a
    | Two a a
    | Many [(Argname, a)]

data Complex a
    = Var           Argname                a
    | MultiIf       [(a, a)]                                            -- if  condition -> a
    | Record        [(Label, a)]           a                            -- { x=2, ... | a }
    | Case a        [(Pattern, a)]                                      -- x | pattern   -> a; _ -> a
    | Lambda        [Argname]              a                            -- \x : Int, y -> x + y  (single product arg)
    | App           a                      (AppArgs a)                  -- f x; f x y; f x=1, y=2, z=3
    --  ??
    | Namespace     [a]                                                 -- ns a.b.x; namespaces define scope

-- sugar and stuff
data Dosyntax a
    = Assign        Argname                 a                           -- x  = a
    | MAssign       Argname                 a                           -- x <- a
    | MExpr         a                                                   -- a

data OpSectionType
    = LeftSection
    | RightSection

data Syntax a
    = InfixApp      a                       a           a               -- x + y; x .then y; x .(f t) y
    | OpSection     OpSectionType           Operator    a               -- \x -> (x op a) or (a op x)
    | LambdaCase    [(Pattern, a)]                                      -- \case pattern -> a
    | IfThenElse    a                       a           a               -- if a then a else a
    --
    | Do            [Dosyntax a]                                        -- do monadicExpr
    | MonadComp     a                       [a]                         -- [x+y | x <- xs, y <- ys, x < y]


-- ifM, do-rec, lazy, deeplazy
-- are record / local-namespace fields defined lazily? Is it ok to recurse in a field?

-- macros
-- typesystem
-- typed literal expression
-- FFI (C, java, haskell)


-- no globals - constructors, record fields, etc
-- record based modules, open in scope
-- typeclasses need to be global

{-
Non global Constructors
    data E a b = L a | R b
    x = E.L 32   : forall b. E Int b                        -- scoped constructors; works well with open sums

Extensible Product, Open Sums
    - Field : Record :: Case : Sum
        - f : {a, b | r}                -> {c   | r}        -- propagate fields information
        - f : (A a | ... | R)           -> (C c | R)        -- propagate case   information

Namespace vs Record
    - namespaces can contain data declarations and other top-level stuff (for compilation)
    - records can only contain fields data (for program logic)

-}


