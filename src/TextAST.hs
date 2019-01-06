{-# LANGUAGE TemplateHaskell #-}

module TextAST where

--
import           Prelude
import           Control.Monad (ap)
import           Data.Functor.Identity (Identity)
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.IntMap     as IM
import           Data.Text       (Text)
import           Text.Parsec     as Tp
import           Text.Parsec.Pos (newPos)
import           Bound
import           Data.Functor.Classes
import           Data.Foldable
import           Data.Traversable
import           Data.Eq.Deriving (deriveEq1)      -- these two are from the
import           Text.Show.Deriving (deriveShow1)  -- deriving-compat package

data Literal
    = QNil
    | QString                   Text
    | QRegex                    Text
    | QRaw                      Text
    | QInt                      Int
    | QFloat                    Double
    | QChar                     Char                      
    | QBool                     Bool
    | QKeyword                  Text
    | QSymbol                   Text
    | QParam                    Text
    | QGensym                   Text
    deriving (Eq, Show)

data TextAST a
    = QLiteral                  Literal
    | QForm                     [Src a]    
    | QList                     [Src a]
    | QVector                   [Src a]
    -- | QMap                      [(Src a, Src a)]
    | QSet                      [Src a]
    | QDeref                    (Src a)
    | QQuote                    (Src a)
    | QBacktick                 (Src a)
    | QUnquote                  (Src a)
    | QUnquoteSplicing          (Src a)
    --
    | QIdiom                    [Src a]    
    | QDo                       [Src a]    
    --
    | QVar                      a
    | QLambda                   (Scope () Src a)
    -- deriving (Eq, Show, Functor, Foldable, Traversable)

data Src a
    = Src { _beg :: SourcePos, _expr :: TextAST a, _end :: SourcePos }
    -- deriving (Eq, Functor, Foldable, Traversable)

-- begin auto derived instances
-- must be written in this way to avoid spurious errors 
-- and to specify instance context (like the `Eq a => stuff`)
deriving instance Eq a => Eq (TextAST a)
deriving instance Show a => Show (TextAST a)
deriving instance Functor TextAST
deriving instance Foldable TextAST
deriving instance Traversable TextAST

deriving instance Eq a => Eq (Src a)
instance Show a => Show (Src a) where
    show (Src b e d) = show e
deriving instance Functor Src
deriving instance Foldable Src
deriving instance Traversable Src

deriveEq1 ''TextAST
deriveShow1 ''TextAST
deriveEq1 ''Src
deriveShow1 ''Src

--
instance Applicative Src where
    pure = noSrc . QVar
    (<*>) = ap
instance Monad Src where
    return = pure

    x >>= f = case _expr x of
        QVar             a -> f a
        QLambda          e -> x { _expr = QLambda          (e >>>= f) }
        -- QMap          a -> x { _expr = QMap             (fmap (>>= f) a) }
        
        QLiteral         _ -> x >>= f
        QForm            a -> x { _expr = QForm            (fmap (>>= f) a) }
        QList            a -> x { _expr = QList            (fmap (>>= f) a) }
        QVector          a -> x { _expr = QVector          (fmap (>>= f) a) }
        QSet             a -> x { _expr = QSet             (fmap (>>= f) a) }
        QIdiom           a -> x { _expr = QIdiom           (fmap (>>= f) a) }
        QDo              a -> x { _expr = QDo              (fmap (>>= f) a) }
        
        QDeref           a -> x { _expr = QDeref           (a >>= f) }
        QQuote           a -> x { _expr = QQuote           (a >>= f) }
        QBacktick        a -> x { _expr = QBacktick        (a >>= f) }
        QUnquote         a -> x { _expr = QUnquote         (a >>= f) }
        QUnquoteSplicing a -> x { _expr = QUnquoteSplicing (a >>= f) }
        
-- basic parser definitions
type Parser a = ParsecT String () Identity a

src :: Parser (TextAST a) -> Parser (Src a)
src p = Src <$> Tp.getPosition <*> p <*> Tp.getPosition
spanSrc x y e = Src (_beg x) e (_end y)
noSrc e = Src noSrcPos e noSrcPos where noSrcPos = newPos "" (-1) (-1)  -- TODO

qNil     = src . fmap (const $ QLiteral QNil)
qString  = src . fmap (QLiteral . QString)
qRegex   = src . fmap (QLiteral . QRegex)
qRaw     = src . fmap (QLiteral . QRaw)
qInt     = src . fmap (QLiteral . QInt)
qFloat   = src . fmap (QLiteral . QFloat)
qChar    = src . fmap (QLiteral . QChar)
qBool    = src . fmap (QLiteral . QBool)
qKeyword = src . fmap (QLiteral . QKeyword)
qSymbol  = src . fmap (QLiteral . QSymbol)
qParam   = src . fmap (QLiteral . QParam)
qGensym  = src . fmap (QLiteral . QGensym)

qLiteral         = src . fmap QLiteral
qForm            = src . fmap QForm
qList            = src . fmap QList
qVector          = src . fmap QVector
-- qMap             = src . fmap QMap
qSet             = src . fmap QSet
-- qLambda          = src .: fmap . QLambda
qDeref           = src . fmap QDeref
qQuote           = src . fmap QQuote
qBacktick        = src . fmap QBacktick
qUnquote         = src . fmap QUnquote
qUnquoteSplicing = src . fmap QUnquoteSplicing

f .: g = \ x y -> f (g x y)
infixr 9 .: