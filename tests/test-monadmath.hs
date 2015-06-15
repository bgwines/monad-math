{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Arbitrary

import MonadMath

-- | A sum type representing objects in the category Hask. Objects
-- in Hask are Haskell types, so any value of type 'HaskellValue' is supposed
-- to represent a Haskell type. One way wonder, then, why under this
-- data type definition we can have multiple values constructed with the
-- same data constructor, instead of something like
--
-- > data HaskellValue = A | B | C -- 'A' represents Int, 'B' represents String, 'C' represents (Char, [Bool]), etc.
--
-- . This is because it is useful to consider different Haskell values
-- of the same type -- we still have the type information, accessible
-- through pattern-matching on the value constructor. However, under
-- this definition, we can test different arrows between the same
-- Hask-objects (arrows in categories are not unique).
data HaskellValue
  = A String
  | B Int
  | C Bool
  | D Char
  | E (Either (Bool, Char, [Bool]) [Maybe (Maybe Int)])
  deriving (Show, Eq)

instance Arbitrary HaskellValue where
    arbitrary :: Gen HaskellValue
    arbitrary = do
        valueConstructor <- choose ('A', 'E')

        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary

        return $ case valueConstructor of {
            'A' -> A a;
            'B' -> B b;
            'C' -> C c;
            'D' -> D d;
            'E' -> E e;
        }

instance CoArbitrary HaskellValue where
    coarbitrary :: HaskellValue -> Gen b -> Gen b
    coarbitrary (A a) = variant 0 . coarbitrary a
    coarbitrary (B b) = variant 1 . coarbitrary b
    coarbitrary (C c) = variant 2 . coarbitrary c
    coarbitrary (D d) = variant 3 . coarbitrary d
    coarbitrary (E e) = variant 4 . coarbitrary e

instance Function HaskellValue where
    --function :: (HaskellValue -> b) -> HaskellValue :-> b
    function = functionMap g h
        where
            g (A a) = (Just a , Nothing, Nothing, Nothing, Nothing)
            g (B b) = (Nothing, Just b , Nothing, Nothing, Nothing)
            g (C c) = (Nothing, Nothing, Just c , Nothing, Nothing)
            g (D d) = (Nothing, Nothing, Nothing, Just d , Nothing)
            g (E e) = (Nothing, Nothing, Nothing, Nothing, Just e )

            h (Just a , Nothing, Nothing, Nothing, Nothing) = A a
            h (Nothing, Just b , Nothing, Nothing, Nothing) = B b
            h (Nothing, Nothing, Just c , Nothing, Nothing) = C c
            h (Nothing, Nothing, Nothing, Just d , Nothing) = D d
            h (Nothing, Nothing, Nothing, Nothing, Just e ) = E e

-- |
-- >          .
-- >  η : I -> F
-- >  h : x -> y
-- >  I : x -> x, the identity endofunctor in the category Hask.
-- >  F : x -> [x], an endofunctor in the category Hask.
-- > 
-- >       I h
-- >  I x ----> I y
-- >  |          |
-- >  | η x      | η y
-- >  |          |
-- >  v          v
-- >  F x -----> F y
-- >       F h
testη :: Fun HaskellValue HaskellValue -> [HaskellValue] -> Bool
testη (Fun _ h) = all
    (\x -> let y = h x in
        (η y . iArr h $ iObj x) == (fArr' h . η x $ iObj x))
    where
        fArr' :: (a -> b) -> ([a] -> [b])
        fArr' = map

-- |
-- >          .
-- >  µ : F^2 -> F
-- >  h : x -> y
-- >  F : x -> [x], an endofunctor in the category Hask.
-- >  F2 : x -> [[x]], an endofunctor in the category Hask, 
-- >                   defined by composition of F with F.
-- > 
-- >       F2 h
-- >  F2 x ----> F2 y
-- >  |          |
-- >  | µ x      | µ y
-- >  |          |
-- >  v          v
-- >  F x -----> F y
-- >       F h
testµ :: Fun HaskellValue HaskellValue -> [HaskellValue] -> Bool
testµ (Fun _ h) = all
    (\x -> let y = h x in
        (µ y . f2Arr h $ [[x]]) == (fArr h . µ x $ [[x]]))

-- | Tests the first monad law: µµf = µfµ
testLaw1 :: HaskellValue -> Bool
testLaw1 a = µµf a [[[a]]] == µfµ a [[[a]]]

-- | Tests the second monad law: µfη = id_F = µηf
testLaw2 :: HaskellValue -> Bool
testLaw2 a = and
    [ iObj [a] == µfη a [a]
    , iObj [a] == µηf a [a] ]
    -- (transitively µfη = µηf)

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 500 } testµ
    quickCheckWith stdArgs { maxSuccess = 500 } testη
    quickCheckWith stdArgs { maxSuccess = 500 } testLaw1
    quickCheckWith stdArgs { maxSuccess = 500 } testLaw2
