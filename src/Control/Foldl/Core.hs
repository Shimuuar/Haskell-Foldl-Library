
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Control.Foldl.Core where

import Control.Category
import Control.Applicative
import Control.Comonad

import Data.Monoid
import Prelude hiding (id,(.))



----------------------------------------------------------------
-- Simple foldl
----------------------------------------------------------------

{-| Efficient representation of a left fold that preserves the fold's step
    function, initial accumulator, and extraction function

    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once
-}
data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINABLE fmap #-}

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)
    {-# INLINABLE pure #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = (doneL xL) (doneR xR)
        in  Fold step begin done
    {-# INLINABLE (<*>) #-}

instance Monoid b => Monoid (Fold a b) where
    mempty = pure mempty
    {-# INLINABLE mempty #-}
    mappend = liftA2 mappend
    {-# INLINABLE mappend #-}

instance Comonad (Fold a) where
  extract (Fold _ x done) = done x
  duplicate (Fold f x done) = Fold f x (\y -> Fold f y done)



----------------------------------------------------------------
-- Monadic fold
----------------------------------------------------------------

-- | Like 'Fold', but monadic
data FoldM m a b = forall x . FoldM (x -> a -> m x) x (x -> m b)

instance (Monad m) => Functor (FoldM m a) where
    fmap f (FoldM step start done) = FoldM step start done'
      where
        done' x = do
            b <- done x
            return $! f b
    {-# INLINABLE fmap #-}

instance (Monad m) => Applicative (FoldM m a) where
    pure b = FoldM (\() _ -> return ()) () (\() -> return b)
    {-# INLINABLE pure #-}
    FoldM stepL beginL doneL <*> FoldM stepR beginR doneR =
        let step (Pair xL xR) a = do
                xL' <- stepL xL a
                xR' <- stepR xR a
                return $! Pair xL' xR'
            begin = Pair beginL beginR
            done (Pair xL xR) = do
                f <- doneL xL
                x <- doneR xR
                return $! f x
        in  FoldM step begin done
    {-# INLINABLE (<*>) #-}

instance (Monoid b, Monad m) => Monoid (FoldM m a b) where
    mempty = pure mempty
    {-# INLINABLE mempty #-}
    mappend = liftA2 mappend
    {-# INLINABLE mappend #-}

-- | We cannot write extract but 'duplicate' is still possible
duplucateM :: Monad m => FoldM m a b -> FoldM m a (FoldM m a b)
duplucateM (FoldM f x done) = FoldM f x (\y -> return $ FoldM f y done)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data Pair a b = Pair !a !b