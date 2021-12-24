{-# Language ScopedTypeVariables #-}
module Unification (unify, UError(..)) where
import Prelude hiding (error)
import Lang
import TypeConstraints
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Result as R


type Result = R.Result Error 

type Error = UError

data UError 
    = InEq Type Type
    | UnboundTyVar TypeName
    deriving (Show, Eq)

type Context a = StateT VarMap Result a

type VarMap = [(TypeName, Type)]

newtype CheckedType = CT Type
    deriving Show

error :: Error -> Context a 
error e = lift $ R.Err e

uncheck (CT t) = t


unify :: [Constraint] -> Type -> Result Type
unify cs t = evalStateT (unifyTopLevel cs t) []

unifyTopLevel :: [Constraint] -> Type -> Context Type
unifyTopLevel cs t = do 
    cs' <- unifyLoop cs
    resolve t 

unifyLoop :: [Constraint] -> Context [Constraint]
unifyLoop [] = return [] 
unifyLoop ((t1,t2):cs) = do 
    ct1 <- find t1
    ct2 <- find t2
    new <- unifyRound ct1 ct2
    unifyLoop $ cs ++ new


unifyRound :: CheckedType -> CheckedType -> Context [Constraint]
unifyRound (CT t1) (CT t2) = 
    case (t1,t2) of
        (Unit, Unit) -> none
        (Integer, Integer) -> none
        (Boolean, Boolean) -> none
        (String, String) -> none
        (Function t1, Function t2) -> return $ zip t1 t2
        (Tuple t1, Tuple t2) -> return $ zip t1 t2
        (Array t1, Array t2) -> return [(t1,t2)]
        (TyVar x, anything) -> do 
            bind x anything
            none
        (anything, TyVar x) -> do
            bind x anything
            none
        (t1,t2) -> error $ InEq t1 t2

    
none :: Context [Constraint]
none = return []


find :: Type -> Context CheckedType 
find (TyVar n) = do 
    env <- get
    case lookup n env of
        Just t -> return $ CT t
        Nothing -> return $ CT $ TyVar n
find t = return $ CT t

resolve :: Type -> Context Type
resolve (TyVar n) = do 
    env <- get
    ans <- lookup n env `R.orElse` error (UnboundTyVar n)
    resolve ans
resolve Unit = return Unit
resolve Integer = return Integer
resolve Boolean = return Boolean
resolve String = return String
resolve (Function ts) = do
    ts' <- forM ts resolve
    return $ Function ts'
resolve (Tuple ts) = do
    ts' <- forM ts resolve
    return $ Tuple ts'
resolve (Array t) = do 
    t' <- resolve t
    return $ Array t

bind :: TypeName -> Type -> Context ()
bind n v = modify (\env -> (n,v):env)
