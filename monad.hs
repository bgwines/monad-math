{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

type HaskObj = Int

--instance Arbitrary HaskObj where
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

-- µ : F^2 -.> F
µ :: a -> ([[a]] -> [a])
µ a = (fObj . fObj $ a) =-> (fObj a)

{-
          h
      x -----> y


         f2 h
    f2 x ----> f2 y
    |          |
    | µ x      | µ y
    |          |
    v          v
    f x -----> f y
         f h

-}
testµ :: (HaskObj, HaskObj) -> Bool
testµ (x, y) = (µ y . f2Arr h $ [[x]]) == (fArr h . µ x $ [[x]])
    where
        h :: HaskObj -> HaskObj
        h = x =-> y

µf :: a -> [[[a]]] -> [[a]]
µf = µ . fObj

fµ :: a -> [[[a]]] -> [[a]]
fµ = fArr . µ

-- law 1: µ • fµ = µ • µf
µµf :: a -> [[[a]]] -> [a]
µfµ :: a -> [[[a]]] -> [a]
µµf = µ • µf
µfµ = µ • fµ

testLaw1 :: HaskObj -> Bool
testLaw1 a = µµf a [[[a]]] == µfµ a [[[a]]]

-- η : I -.> F
η :: a -> (a -> [a])
η a = a =-> fObj a

{-
          h
      x -----> y


         i h
    i x ----> i y
    |          |
    | η x      | η y
    |          |
    v          v
    f x -----> f y
         f h

-}
testη :: (HaskObj, HaskObj) -> Bool
testη (x, y) = (η y . id h $ id x) == (fArr' h . η x $ id x)
    where
        h :: HaskObj -> HaskObj
        h = x =-> y

        fArr' :: (a -> b) -> ([a] -> [b])
        fArr' = map

fη :: a -> [a] -> [[a]]
fη = fArr . η

ηf :: a -> [a] -> [[a]]
ηf = η . fObj

-- law 2: µηf = id_F = µfη
µηf :: a -> ([a] -> [a])
µfη :: a -> ([a] -> [a])
µηf = µ • ηf
µfη = µ • fη

testLaw2 :: HaskObj -> Bool
testLaw2 a = and
    [ id [a] == µfη a [a]
    , id [a] == µηf a [a] ]
    -- (transitively µfη = µηf)

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 50 } testµ
    quickCheckWith stdArgs { maxSuccess = 50 } testη
    quickCheckWith stdArgs { maxSuccess = 50 } testLaw1
    quickCheckWith stdArgs { maxSuccess = 50 } testLaw2
