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
--a =-> b = \a' -> if a == a' then b else undefined

-- vertical composition
(•) :: (a -> (ga -> ha)) -> (a -> (fa -> ga)) -> (a -> (fa -> ha))
µ • η = \a -> µ a . η a

-- natural transformations are composed vertically since
-- they have type `a -> ([a] -> G a)`, and will be written -.>

-- F, the Hask endofunctor.
fObj :: (Functor f, Applicative f) => a -> f a
fObj = pure

fArr :: (Functor f, Applicative f) => (a -> b) -> (f a -> f b)
fArr = fmap

f2Obj :: (Functor f, Applicative f) => a -> f (f a)
f2Obj = fObj . fObj

f2Arr :: (Functor f, Applicative f) => (a -> b) -> (f (f a) -> f (f b))
f2Arr = fArr . fArr

-- the natural transformations

-- µ : F^2 -.> F
µ :: (Functor f, Applicative f) => a -> (f (f a) -> f a)
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

µf :: (Functor f, Applicative f) => a -> (f (f (f a)) -> f (f a))
µf = µ . fObj

fµ :: (Functor f, Applicative f) => a -> (f (f (f a)) -> f (f a))
fµ = fArr . µ

-- law 1: µ • fµ = µ • µf
µµf :: (Functor f, Applicative f) => a -> (f (f (f a)) -> f a)
µfµ :: (Functor f, Applicative f) => a -> (f (f (f a)) -> f a)
µµf = µ • µf
µfµ = µ • fµ

testLaw1 :: HaskObj -> Bool
testLaw1 a = µµf a [[[a]]] == µfµ a [[[a]]]

-- η : I -.> F
η :: (Functor f, Applicative f) => a -> (a -> f a)
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

fη :: (Functor f, Applicative f) => a -> f a -> f (f a)
fη = fArr . η

ηf :: (Functor f, Applicative f) => a -> f a -> f (f a)
ηf = η . fObj

-- law 2: µηf = id_F = µfη
µηf :: (Functor f, Applicative f) => a -> (f a -> f a)
µfη :: (Functor f, Applicative f) => a -> (f a -> f a)
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
