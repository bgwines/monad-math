{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadMath
(
-- * data types
  HaskObj(..)

-- * combinators
, (•)
, (=->)

-- * functors
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

-- TODO: positive
-- 0 represents Int
-- 1 represents [Int]
-- 2 represents [[Int]]
-- ...
type HaskObj = Int

--instance Arbitrary (HaskObj -> HaskObj) where
--    arbitrary :: Gen HaskObj
--    arbitrary = do
--        x <- choose (False, True)
--        return (if x == True then TypeA else TypeB)

--instance (Applicative f) => Eq (f HaskObj) where
--    (==) :: f HaskObj -> f HaskObj -> Bool
--    fa == fb = liftA2 (==) fa fb == pure True

-- combinators

-- builds a function from `a` to `b`. In our case, our values are of
-- type HaskObj (or some number of lists wrapping that type), and each
-- value of that type represents a type in the category Hask, so the
-- value constructors are one-to-one with types.
(=->) :: a -> b -> (a -> b)
a =-> b = \a' -> b
--a =-> b = \a' -> i[a] == a' then b else undefined

-- vertical composition
(•) :: (a -> (ga -> ha)) -> (a -> (fa -> ga)) -> (a -> (fa -> ha))
µ • η = \a -> µ a . η a

-- natural transformations are composed vertically since
-- they have type `a -> ([a] -> G a)`, and will be written -.>

-- F, the Hask endofunctor.
fObj :: a -> [a]
fObj = pure

fArr :: (a -> b) -> ([a] -> [b])
fArr = fmap

f2Obj :: a -> [[a]]
f2Obj = fObj . fObj

f2Arr :: (a -> b) -> ([[a]] -> [[b]])
f2Arr = fArr . fArr

-- the natural transformations

--α :: 

-- µ : F^2 -.> F
µ :: a -> ([[a]] -> [a])
µ a = (fObj . fObj $ a) =-> (fObj a)

µf :: a -> [[[a]]] -> [[a]]
µf = µ . fObj

fµ :: a -> [[[a]]] -> [[a]]
fµ = fArr . µ

-- law 1: µ • fµ = µ • µf
µµf :: a -> [[[a]]] -> [a]
µfµ :: a -> [[[a]]] -> [a]
µµf = µ • µf
µfµ = µ • fµ

-- η : I -.> F
η :: a -> (a -> [a])
η a = a =-> fObj a

fη :: a -> [a] -> [[a]]
fη = fArr . η

ηf :: a -> [a] -> [[a]]
ηf = η . fObj

-- law 2: µηf = id_F = µfη
µηf :: a -> ([a] -> [a])
µfη :: a -> ([a] -> [a])
µηf = µ • ηf
µfη = µ • fη

