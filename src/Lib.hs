module Lib () where

--
import           Control.Applicative     ((<|>))
import           Text.Parser.Combinators ((<?>))
import           Text.Parser.Expression
import           Text.Parser.Token       (TokenParsing, natural, parens,
                                          reserve)
import           Text.Parser.Token.Style (emptyOps)

expr   :: (Monad m, TokenParsing m) => m Integer
expr    = buildExpressionParser table term
        <?> "expression"

term   :: (Monad m, TokenParsing m) => m Integer
term    =  parens expr
        <|> natural
        <?> "simple expression"

table  :: (Monad m, TokenParsing m) => [[Operator m Integer]]
table = [ [prefix "-" negate, prefix "+" id ]
        , [postfix "++" (+1)]
        , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
        , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
        ]

binary  name fun assoc = Infix (fun <$ reservedOp name) assoc
prefix  name fun       = Prefix (fun <$ reservedOp name)
postfix name fun       = Postfix (fun <$ reservedOp name)

reservedOp name = reserve emptyOps name
