{-# Language DeriveFunctor #-}
module Result where
import Control.Monad
import Control.Monad.State

data Result error a 
    = Ok a
    | Err error
    deriving (Show, Eq, Ord, Functor)

instance Applicative (Result e) where
    pure = Ok
    Ok f <*> Ok a = Ok $ f a
    Ok _ <*> (Err e) = Err e
    (Err e) <*> _ = Err e

instance Monad (Result e) where
    Ok a >>= f = f a
    (Err e) >>= _ = Err e


orElse :: Monad m => Maybe a -> m a -> m a
orElse (Just x) _ = return x
orElse Nothing els = els
