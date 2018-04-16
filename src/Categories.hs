{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Categories where

import           Prelude hiding (const, curry, uncurry)

--
infixr 9 ◦
class Category k where
    id  :: a `k` a
    (◦) :: b `k` c -> a `k` b -> a `k` c

instance Category (->) where
    id x  = x
    g ◦ f = \x -> g (f x)

infixr 3 △
class Category k => Cartesian k where
    (△) :: a `k` c       -> a `k` d -> a `k` (c, d)
    _1  :: (a, b) `k` a
    _2  :: (a, b) `k` b

    -- each category can have its own product construction
    -- objects/types of that category need not be kind *
    -- universal property for a Cartesian k
        -- ∀h. h ≡ f △ g ⇐=> exl ◦ h ≡ f ∧ exr ◦ h ≡ g

instance Cartesian (->) where
    f △ g = \x -> (f x, g x)
    _1 (a,_) = a
    _2 (_,b) = b

--
type Unit = ()

class Category k => Terminal k where
    it :: a `k` Unit

instance Terminal (->) where
    it _ = ()

--
type (|=>) = (->)

class Cartesian k => Closed k where
    apply   :: (a |=> b, a)     `k`   b
    curry   :: (a, b) `k` c      ->   a `k` (b |=> c)
    uncurry :: a `k` (b |=> c)   ->   (a, b) `k` c

instance Closed (->) where
    apply  (f, x) = f x
    curry   f     = \a b    -> f (a, b)
    uncurry f     = \(a, b) -> f a b

    -- each CCC can have its own notion of exponentials.
    -- The universal property:
        -- apply ◦ (curry f ◦ _1 △ _2) ≡ f

--
class Terminal k => ConstCat k b where
    unitArrow :: b -> Unit `k` b

instance ConstCat (->) b where
    unitArrow b = \() -> b

const :: ConstCat k b => b -> a `k` b
const b = unitArrow b ◦ it

--
infixr 2 ▽
class Category k => Cocartesian k where
    inl :: a `k` (Either a b)
    inr :: b `k` (Either a b)
    (▽) :: (c `k` a) -> (d `k` a) -> (Either c d) `k` a

instance Cocartesian (->) where
    inl = Left
    inr = Right
    (f ▽ g) (Left a)  = f a
    (f ▽ g) (Right b) = g b

--
class (Cartesian k, Cocartesian k) => Distrib k where
    distl :: (a, Either u v)      `k` Either (a, u) (a, v)
    distr :: Either (a, u) (a, v) `k` (a, Either u v)

instance Distrib (->) where
    distl (a, Left u)  = Left  (a, u)
    distl (a, Right v) = Right (a, v)
    distr (Left  (a, u)) = (a, Left u)
    distr (Right (a, v)) = (a, Right v)


-- Function-valued primitives may have interpretations in other categories, -- which can be captured in additional ad hoc Category subclasses
class Cartesian k => BoolCat k where
    notC :: Bool `k` Bool
    andC, orC :: (Bool, Bool) `k` Bool

class NumCat k a where
    negateC :: a `k` a
    addC, mulC :: (a, a) `k` a

instance BoolCat (->) where
    notC = not
    andC = uncurry (&&)
    orC  = uncurry (||)

instance Num a => NumCat (->) a where
    negateC = negate
    addC = uncurry (+)
    mulC = uncurry (*)
