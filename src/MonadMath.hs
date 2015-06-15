{-# LANGUAGE ConstraintKinds #-}

-- | A module exposing the structure of monads.
module MonadMath
(
-- * data types
  Functor'(..)

-- * combinators
, (•)

-- * functors
, iObj
, iArr
, fObj
, fArr
, f2Obj
, f2Arr

-- * natural transformations
, µ
, η

-- * compositions of natural transformations and functors
, µf
, fµ
, ηf
, fη

-- * compositions of natural transformations
, µµf
, µfµ
, µηf
, µfη
) where

import Control.Applicative
import Control.Monad

-- A category-theoretic functor. Has both `pure` and `fmap`, and equality
-- on liftings (trunctated here at depth three, the minimum necessary for
-- monads).
type Functor' f a = (Eq a, Applicative f, Eq (f a), Eq (f (f a)), Eq (f (f (f a))))

-- | Builds a function from value `a' to value `b'. The returned
-- function yields `undefined' when given anything other than the
-- value `a`
(=->) :: (Eq a) => a -> b -> (a -> b)
a =-> b = \a' -> if a' == a then b else undefined

-- | Vertical composition of functions:
--
-- > µ • η = \a -> µ a . η a
(•) :: (a -> (ga -> ha)) -> (a -> (fa -> ga)) -> (a -> (fa -> ha))
µ • η = \a -> µ a . η a

-- | The identity functor on Hask as it behaves on objects.
iObj :: a -> a
iObj = id

-- | The identity functor on Hask as it behaves on arrows.
iArr :: (a -> b) -> (a -> b)
iArr = id

-- | F, a Hask endofunctor, as it behaves on objects.
fObj :: (Applicative f) => a -> f a
fObj = pure

-- | F, a Hask endofunctor, as it behaves on arrows.
fArr :: (Applicative f) => (a -> b) -> (f a -> f b)
fArr = fmap

-- | F^2 = F . F, a Hask endofunctor, as it behaves on objects.
f2Obj :: (Applicative f) => a -> f (f a)
f2Obj = fObj . fObj

-- | F^2 = F . F, a Hask endofunctor, as it behaves on arrows.
f2Arr :: (Applicative f) => (a -> b) -> (f (f a) -> f (f b))
f2Arr = fArr . fArr

-- natural transformations

-- |
-- >          .
-- >  µ : F^2 -> F, a natural transformation
-- >  h : x -> y, a function
-- >  F : x -> F x, an endofunctor in the category Hask.
-- >  F2 : x -> F F x, an endofunctor in the category Hask.
-- > 
-- >       F2 h
-- >  F2 x ----> F2 y
-- >  |          |
-- >  | µ x      | µ y
-- >  |          |
-- >  v          v
-- >  F x -----> F y
-- >       F h
µ :: (Eq a, Applicative f, Eq (f a), Eq (f (f a))) => a -> (f (f a) -> f a)
µ a = (fObj . fObj $ a) =-> (fObj a)

-- |
-- >        .
-- >  η : I -> F, a natural transformation
-- >  h : x -> y, a function
-- >  I : x -> x, the identity endofunctor in the category Hask.
-- >  F : x -> F x, an endofunctor in the category Hask.
-- > 
-- >       I h
-- >  I x ----> I y
-- >  |          |
-- >  | η x      | η y
-- >  |          |
-- >  v          v
-- >  F x -----> F y
-- >       F h
η :: (Eq a, Applicative f) => a -> (a -> f a)
η a = iObj a =-> fObj a

-- * compositions of natural transformations and functors

-- | The natural transformation formed by composition of µ with F.
µf :: Functor' f a => a -> (f (f (f a)) -> f (f a))
µf = µ . fObj

-- | The natural transformation formed by composition of F with µ.
fµ :: Functor' f a => a -> (f (f (f a)) -> f (f a))
fµ = fArr . µ

-- | The natural transformation formed by composition of F with η.
fη :: Functor' f a => a -> (f a -> f (f a))
fη = fArr . η

-- | The natural transformation formed by composition of η with F.
ηf :: Functor' f a => a -> (f a -> f (f a))
ηf = η . fObj

-- * compositions of natural transformations and natural transformations

-- | The natural transformation formed by composition of µ with µf.
µµf :: Functor' f a => a -> (f (f (f a)) -> f a)
µµf = µ • µf

-- | The natural transformation formed by composition of µ with fµ.
µfµ :: Functor' f a => a -> (f (f (f a)) -> f a)
µfµ = µ • fµ

-- | The natural transformation formed by composition of µ with ηf.
µηf :: Functor' f a => a -> (f a -> f a)
µηf = µ • ηf

-- | The natural transformation formed by composition of µ with fη.
µfη :: Functor' f a => a -> (f a -> f a)
µfη = µ • fη
