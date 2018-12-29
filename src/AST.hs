{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module AST where

--
import           Prelude         (Char, Bool, Int, Double, Maybe, Show, show, Functor, (++))
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.IntMap     as IM
import           Data.Text       (Text)
import           Text.Parsec     as Tp

data Literal
    = QString                   Text
    | QRegex                    Text
    | QRaw                      Text
    | QInt                      Int
    | QFloat                    Double
    | QChar                     Char
    | QNil                      
    | QBool                     Bool
    | QKeyword                  Text
    | QSymbol                   Text
    | QParam                    Text
    | QGensym                   Text
    deriving (Show)

data TextAST a
    = QLiteral                  Literal
    | QForm                     [a]    
    | QList                     [a]
    | QVector                   [a]
    | QMap                      [(a, a)]
    | QSet                      [a]
    | QLambda                   [a] a
    | QDeref                    a
    | QQuote                    a
    | QBacktick                 a
    | QUnquote                  a
    | QUnquoteSplicing          a
    --
    | QIdiom                    [a]    
    | QDo                       [a]    
    deriving (Show, Functor)

data Src f a
    = Src { _beg :: SourcePos, _expr :: f (Src f a), _end :: SourcePos }
    deriving (Functor)

type TextExpr a = Src TextAST a
instance (Show a, Show (f (Src f a))) => Show (Src f a) where
    show (Src b e d) = show e




