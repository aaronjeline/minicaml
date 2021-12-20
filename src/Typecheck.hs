{-# Language ScopedTypeVariables #-}
module Typecheck 
(CheckedExp, typecheck, getexp, getType)
where

import Lang
import TypeConstraints
import Unification

newtype CheckedExp = CE Exp deriving Show

getexp :: CheckedExp -> Exp
getexp (CE e) = e

getType :: Exp -> Maybe Type
getType e = fst <$> typecheck e

typecheck :: Exp -> Maybe (Type, CheckedExp)
typecheck e = 
    case doConstrain e of
        Ok (t, cs) -> do
            t' <- unify cs t
            return (t', CE e)
        _ -> Nothing


