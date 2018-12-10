{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Turtle as X

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- composition
(.:) :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
f .: g = \ a b -> f (g a b)

(.:.) :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
f .:. g = \ a b c -> f (g a b c)

-- string formatting and printing
fmt :: Format Text r -> r
fmt = format

show :: Show a => a -> T.Text
show = T.pack . Prelude.show

-- string conversions
class Convertible a b where
    conv :: a -> b

instance Convertible [Char] T.Text where conv = T.pack
instance Convertible T.Text [Char] where conv = T.unpack
instance Convertible [Char] Line where conv = fromString
instance Convertible Line [Char] where conv = T.unpack . conv

instance Convertible T.Text Line where conv = unsafeTextToLine
instance Convertible Line T.Text where conv = lineToText
instance Convertible T.Text X.FilePath where conv = fromText
instance Convertible X.FilePath T.Text where conv = Utils.show
instance Convertible T.Text BS.ByteString where conv = encodeUtf8
instance Convertible BS.ByteString T.Text where conv = decodeUtf8
instance Convertible X.FilePath Line where conv = conv . Prelude.show
instance Convertible Line X.FilePath where conv = conv . (conv :: Line -> Text)

instance Convertible Int BS.ByteString where conv = conv . Utils.show

-- instance (Functor f, Convertible a b) => Convertible (f a) (f b) where conv = fmap conv





-- 
fmap_ :: Functor f => f a -> (a -> b) -> f b
fmap_ = flip fmap

ifM mb mt mf = do
    b <- mb
    if b then mt else mf

ifM_ mb mt = ifM mb mt $ return ()

unlessM mb = ifM (not <$> mb)
unlessM_ mb mt = unlessM mb mt $ return ()