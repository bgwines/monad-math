
import Control.Applicative
import Control.Monad
import Test.QuickCheck

-- vertical composition
(•) :: (a -> (ga -> ha)) -> (a -> (fa -> ga)) -> (a -> (fa -> ha))
µ • η = \a -> µ a . η a

-- natural transformations are composed vertically since
-- they have type `a -> ([a] -> G a)`, and will be written -.>

-- F, the Hask endofunctor.
fObj :: (Eq a) => a -> [a]
fObj = pure

fArr :: (Eq a) => (a -> b) -> ([a] -> [b])
fArr = fmap

-- the natural transformations

-- µ : F^2 -.> F
µ :: (Eq a) => a -> ([[a]] -> [a])
µ _ = join

µf :: (Eq a) => a -> ([[[a]]] -> [[a]])
µf = µ . fObj

fµ :: (Eq a) => a -> ([[[a]]] -> [[a]])
fµ = fArr . µ

-- law 1: µ • fµ = µ • µf
µµf :: (Eq a) => a -> ([[[a]]] -> [a])
µfµ :: (Eq a) => a -> ([[[a]]] -> [a])
µµf = µ • µf
µfµ = µ • fµ

-- we use () because its values are 1:1 and onto with its type (objects
-- in the category Hask are types so we want the code to mirror that)
testLaw1 :: [[[()]]] -> Bool
testLaw1 a = µµf () a == µfµ () a

-- η : I -.> F
η :: (Eq a) => a -> (a -> [a])
η _ = fObj

fη :: (Eq a) => a -> [a] -> [[a]]
fη = fArr . η

ηf :: (Eq a) => a -> [a] -> [[a]]
ηf = η . fObj

-- law 2: µηf = id_F = µfη
µηf :: (Eq a) => a -> ([a] -> [a])
µfη :: (Eq a) => a -> ([a] -> [a])
µηf = µ • ηf
µfη = µ • fη

testLaw2 :: [()] -> Bool
testLaw2 a = and
    [ id a == µfη () a
    , id a == µηf () a ]
    -- (transitively µfη = µηf)

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 50 } testLaw1
    quickCheckWith stdArgs { maxSuccess = 50 } testLaw2
