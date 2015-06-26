{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A module exposing the structure of monads.
module MonadMath
( Monad'(..)
) where

import Control.Applicative

class (Applicative m) => Monad' m where
    -- | Defines how `m`'s functor lifts objects
    fObj :: a -> m a
    fObj = pure

    -- | Defines how `m`'s functor lifts arrows
    fArr :: (a -> b) -> (m a -> m b)
    fArr = fmap

    -- natural transformations

    -- | F^2, as it applies to objects
    f2Obj :: a -> m (m a)
    f2Obj = fObj . fObj

    -- | F^2, as it applies to arrows
    f2Arr :: (a -> b) -> (m (m a) -> m (m b))
    f2Arr = fArr . fArr

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
    µ :: a -> (m (m a) -> m a)

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
    η :: a -> (a -> m a)
    η _ = fObj

    -- * compositions of natural transformations and functors

    -- | The natural transformation formed by composition of µ with F.
    µf :: a -> (m (m (m a)) -> m (m a))
    µf = µ . fObj

    -- | The natural transformation formed by composition of F with µ.
    fµ :: a -> (m (m (m a)) -> m (m a))
    fµ = fArr . µ

    -- | The natural transformation formed by composition of F with η.
    fη :: a -> (m a -> m (m a))
    fη = fArr . η

    -- | The natural transformation formed by composition of η with F.
    ηf :: a -> (m a -> m (m a))
    ηf = η . fObj

    -- * compositions of natural transformations and natural transformations

    -- | The natural transformation formed by composition of µ with µf.
    µµf :: a -> (m (m (m a)) -> m a)
    µµf = µ • µf

    -- | The natural transformation formed by composition of µ with fµ.
    µfµ :: a -> (m (m (m a)) -> m a)
    µfµ = µ • fµ

    -- | The natural transformation formed by composition of µ with ηf.
    µηf :: a -> (m a -> m a)
    µηf = µ • ηf

    -- | The natural transformation formed by composition of µ with fη.
    µfη :: a -> (m a -> m a)
    µfη = µ • fη

-- | Vertical composition of functions:
--
-- > µ • η = \a -> µ a . η a
(•) :: (a -> (ga -> ha)) -> (a -> (fa -> ga)) -> (a -> (fa -> ha))
µ • η = \a -> µ a . η a

-- | The identity functor as it behaves on objects.
iObj :: a -> a
iObj = id

-- | The identity functor as it behaves on arrows.
iArr :: (a -> b) -> (a -> b)
iArr = id

instance Monad' [] where
    µ :: a -> ([[a]] -> [a])
    µ _ = concat

    η :: a -> (a -> [a])
    η _ = fObj

instance Monad' Maybe where
    µ :: a -> (Maybe (Maybe a) -> Maybe a)
    µ _ (Just (Just a)) = Just a
    µ _ (Just Nothing) = Nothing
    µ _ Nothing = Nothing

    η :: a -> (a -> Maybe a)
    η _ = fObj
