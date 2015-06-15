
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import MonadMath

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
-- (3, 4, f)
-- then f : [[[Int]]] -> [[[[Int]]]]
-- for example, f could be (replicate 3)
--                      or (replicate 8)
--
-- (4, 3, f)
-- then f : [[[[Int]]]] -> [[[Int]]]
-- for example, f could be concat
--                      or (\x -> if null x then [] else head x)
--                      or (\x -> if ... x then [] else x !! 2)
testµ :: (HaskObj, HaskObj, HaskObj{- -> HaskObj-}) -> Bool
testµ (x, y, h) = (µ y . f2Arr h' $ [[x]]) == (fArr h' . µ x $ [[x]])
    where
        h' = h =-> h

testLaw1 :: HaskObj -> Bool
testLaw1 a = µµf a [[[a]]] == µfµ a [[[a]]]

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
