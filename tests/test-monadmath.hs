{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Arbitrary

import MonadMath

-- | A sum type representing objects in the category Hask. Objects
-- in Hask are Haskell types, so any value of type 'HV' is supposed
-- to represent a Haskell type. One way wonder, then, why under this
-- data type definition we can have multiple values constructed with the
-- same data constructor, instead of something like
--
-- > data HV = A | B | C -- 'A' represents Int, 'B' represents String, 'C' represents (Char, [Bool]), etc.
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

type HV = HaskellValue

iObj :: a -> a
iObj = id

iArr :: (a -> b) -> (a -> b)
iArr = id

type MonadImpl m = (Monad' m, Eq (m HV)) => (HV -> HV) -> (m HV -> m HV)

instance Arbitrary HV where
    arbitrary :: Gen HV
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

-- limit list sizes to make test cases not run forever
instance Arbitrary HV => Arbitrary [HV] where
    arbitrary = sized $ \_ ->
        do
            k <- choose ((0, 10) :: (Int, Int))
            sequence [ arbitrary | _ <- [1..k] ]

-- limit list sizes to make test cases not run forever
instance Arbitrary [HV] => Arbitrary [[HV]] where
    arbitrary = sized $ \_ ->
        do
            k <- choose ((0, 10) :: (Int, Int))
            sequence [ arbitrary | _ <- [1..k] ]

instance CoArbitrary HV where
    coarbitrary :: HV -> Gen b -> Gen b
    coarbitrary (A a) = variant 0 . coarbitrary a
    coarbitrary (B b) = variant 1 . coarbitrary b
    coarbitrary (C c) = variant 2 . coarbitrary c
    coarbitrary (D d) = variant 3 . coarbitrary d
    coarbitrary (E e) = variant 4 . coarbitrary e

instance Function HV where
    --function :: (HV -> b) -> HV :-> b
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
testη :: (Monad' m, Eq (m HV)) => MonadImpl m -> Fun HV HV -> [HV] -> Bool
testη fArr' (Fun _ h) = all
    (\x -> let y = h x in
        (η y . iArr h $ iObj x) == (fArr' h . η x $ iObj x))

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
testµ
    :: (Monad' m, Eq (m HV))
    => MonadImpl m
    -> Fun HV HV
    -> [m (m HV)]
    -> Bool
testµ _ (Fun _ h) = all
    (\mma ->
        (µ undefined . f2Arr h $ mma) == (fArr h . µ undefined $ mma))

-- | Tests the first monad law: µµf = µfµ
testLaw1 :: (Monad' m, Eq (m HV)) => MonadImpl m -> m (m (m HV)) -> Bool
testLaw1 _ mmma = µµf undefined mmma == µfµ undefined mmma

-- | Tests the second monad law: µfη = id_F = µηf
testLaw2 :: (Monad' m, Eq (m HV)) => MonadImpl m -> m HV -> Bool
testLaw2 _ ma = and
    [ iObj ma == µfη undefined ma
    , iObj ma == µηf undefined ma ]
    -- (transitively µfη = µηf)

testMonad ::
    ( Monad' m
    , Arbitrary (m HV)
    , Arbitrary (m (m HV))
    , Arbitrary (m (m (m HV)))
    , Show (m HV)
    , Show (m (m HV))
    , Show (m (m (m HV)))
    , Eq (m HV) )
    => MonadImpl m -> IO ()
testMonad monadImpl = do
    quickCheckWith stdArgs { maxSuccess = 25 } (testµ monadImpl)
    quickCheckWith stdArgs { maxSuccess = 25 } (testη monadImpl)
    quickCheckWith stdArgs { maxSuccess = 25 } (testLaw1 monadImpl)
    quickCheckWith stdArgs { maxSuccess = 25 } (testLaw2 monadImpl)

listImpl :: MonadImpl []
listImpl = fmap

maybeImpl :: MonadImpl Maybe
maybeImpl = fmap

main :: IO ()
main = do
    testMonad listImpl
    testMonad maybeImpl
