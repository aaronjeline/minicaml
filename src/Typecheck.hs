{-# Language ScopedTypeVariables #-}
module Typecheck 
(CheckedExp, typecheck, getexp, getType)
where

import Lang
import TypeConstraints
import Unification
import qualified Result as R

type Result = R.Result (Either TCError UError)

newtype CheckedExp = CE Exp deriving Show

getexp :: CheckedExp -> Exp
getexp (CE e) = e

getType :: Exp -> Result Type
getType e = fst <$> typecheck e

liftTC :: R.Result TCError a -> Result a
liftTC (R.Ok a) = return a
liftTC (R.Err e) = R.Err $ Left e

liftU :: R.Result UError a -> Result a
liftU (R.Ok a) = return a
liftU (R.Err e) = R.Err $ Right e


typecheck :: Exp -> Result (Type, CheckedExp)
typecheck e =  do
    (t, cs) <- liftTC $ doConstrain e
    t' <- liftU $ unify cs t
    return (t', CE e)



