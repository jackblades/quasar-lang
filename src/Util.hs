


module Util where

import Prelude

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 0 &

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f .: g = \ x y -> f (g x y)
infixr 9 .:

-- fmapN f = fmap (fmap (<n-2>times-more-fmap f))
fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 f = fmap (fmap f)
