
module Parser2 where

--
import           Control.Applicative     ((<|>))
import           AST2
import           Lexer
import           Text.Parsec
import           Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import qualified Data.Text as T

import qualified Data.Map as M
import qualified Data.IntMap as IM

import           Debug.Trace (trace)
import Data.Functor.Identity (Identity)

--
expr = buildExpressionParser optable term

-- TODO generate this from source
-- optable :: [[Text.Parsec.Expr.Operator String u Data.Functor.Identity.Identity Src]]
-- (\x y -> spanSrc x y (Apply "$" $ spanSrc x y (QTuple $ IM.fromList [(0,x), (1,y)])))

app2 op x y = 
    spanSrc x y 
        $ Apply [noSrc $ VAR $ T.pack op, x, y]

optable = 
    [ [ binary (lexsym "<|") AssocRight (app2 "<|")
      , binary (lexsym "|>") AssocLeft (app2 "|>")
      ]
    ]
   -- <?> "expression"

--
term = Apply <$> many1 term2
term2 = choice
    [ parensExpr
    -- , bracesExpr
    -- , bracketsExpr
    ]

parensExpr = parens $ choice [unit, tuple, expr]
-- bracesExpr = return UNIT
-- bracketsExpr = return UNIT









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
-- coroutines -- pipes, conduit etc
