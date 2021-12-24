{-# Language DuplicateRecordFields, DeriveFunctor, ScopedTypeVariables, TupleSections, NamedFieldPuns, FlexibleInstances #-}
module TypeConstraints 
(doConstrain, printConstraints, TCError(..), Constraint, allFV)
where
import Lang
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Result
import qualified Data.Set as S

type ConstraintResult = (Type, [Constraint])
type Constraint = (Type, Type)
type Context a = ReaderT TyEnv (StateT Generator (Result Error)) a
type TyEnv = [(Name, Type)]
type Generator = Int

allFV :: [Constraint] -> S.Set TypeName
allFV cs = S.unions $ (\(t1,t2) -> freeTVars t1 `S.union` freeTVars t2) <$> cs

printConstraints :: [Constraint] -> IO ()
printConstraints cs = forM_ cs printConstraint

    
printConstraint :: Constraint -> IO () 
printConstraint (t1,t2) = putStr s
    where s = show t1 ++ " = " ++ show t2 ++ "\n"

doConstrain :: Exp -> Result Error (Type, [Constraint])
doConstrain e = do 
    (r, cs) <- evalStateT (runReaderT (buildConstraints e) newenv) newgenerator
    return (r, cs)

buildConstraints :: Exp -> Context ConstraintResult
buildConstraints U = return (Unit, [])
buildConstraints (Int _) = return (Integer, [])
buildConstraints (Bool _) = return (Boolean, [])
buildConstraints (Str _) = return (String, [])
buildConstraints (Arith o xs) =
    if null xs then
        mismatch Integer Integer
    else do
        cs <- forM xs buildConstraints
        let cs' = mconcat $ map snd cs
        let ints = Integer : ints
        let cs'' = zip ints $ map fst cs
        return (if o == Eq then Boolean else Integer, cs'' ++ cs')
buildConstraints (If e e1 e2) = do 
    (t, cs) <- buildConstraints e
    (t1, cs1) <- buildConstraints e1
    (t2, cs2) <- buildConstraints e2
    let cs' = mconcat [cs,cs1,cs2]
    case (t == Boolean, t1 == t2) of
        (True, True) -> return (t1, cs')
        (False, _) -> mismatch t Boolean
        (True, False) -> mismatch t1 t2
buildConstraints (Variable x) = (, []) <$> findVar x
buildConstraints (Let x def body) = do
    xv <- fresh
    (t, cs') <- local (bind x xv) (buildConstraints def)
    (tb, cs'') <- local (bind x t) (buildConstraints body) 
    let cs = (xv,t) : mconcat [cs', cs'']
    return (tb, cs)
buildConstraints l@(Lam params body) = do
    argVars <- forM params (const fresh)
    (retT, cs) <- local (bindAll $ zip params argVars) (buildConstraints body)
    return (Function (argVars ++ [retT]), cs)
buildConstraints (Letrec defs body) = do 
    (binds, cs) <- constrainDefs defs 
    (t, cs') <- local (bindAll binds) $ buildConstraints body
    return (t, cs ++ cs')
buildConstraints (f :@ args) = do 
    (ft, cs) <- buildConstraints f
    (argTs, cs) <- flipTuple <$> forM args buildConstraints
    ret <- fresh
    let new = (ft, Function $ argTs ++ [ret])
    let cs' = new : cs ++ cs
    return (ret, cs')
buildConstraints (MakeTuple es) = do 
    (ts, cs) <- flipTuple <$> forM es buildConstraints
    return (Tuple ts, cs)
buildConstraints BindTuple { names, tuple, body } = do 
    (tt, cs) <- buildConstraints tuple
    freshBinds <- forM names (const fresh)
    let new = (tt, Tuple freshBinds)
    let env' = zip names freshBinds
    (tb, cs') <- local (bindAll env') (buildConstraints body)
    return (tb, new : cs ++ cs')
buildConstraints MakeArray { count, init } = do 
    (countT, cs) <- buildConstraints count
    (initT, cs') <- buildConstraints init
    return (Array initT, (countT, Integer) : cs ++ cs')
buildConstraints ArrayRead { idx, arr } = do 
    contents <- fresh
    let arrayT = Array contents
    (idxT, cs') <- buildConstraints idx
    (arrT, cs'') <- buildConstraints arr 
    let cs = (idxT, Integer) : (arrT, arrayT) : cs' ++ cs''
    return (contents, cs)
buildConstraints ArrayWrite { idx, arr, val } = do 
    (idxT, cs') <- buildConstraints idx
    (arrT, cs'') <- buildConstraints arr 
    (valT, cs''') <- buildConstraints val
    let cs = (idxT, Integer) : (arrT, Array valT) : cs' ++ cs'' ++ cs'''
    return (Unit, cs)


flipTuple :: [(a,[b])] -> ([a], [b])
flipTuple lst = (map fst lst, mconcat $ map snd lst)

constrainDef :: Def -> Context (Name, Type, [Constraint])
constrainDef Def { name, params, body } = do 
    f <- fresh
    params_vars <- forM params (const fresh)
    let binds = (name,f) : zip params params_vars
    (t, cs) <- local (bindAll binds) (buildConstraints body)
    let func_const = (f, Function (params_vars ++ [t]))
    return (name, f, func_const : cs)

constrainDefs :: [Def] -> Context ([(Name, Type)], [Constraint])
constrainDefs defs = do
    defs_ts <- forM defs constrainDef
    let names = map first defs_ts
    let types = map second defs_ts
    let cs = mconcat $ map third defs_ts
    let binds = zip names types
    return (binds, cs)

first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c) -> b
second (_,b,_) = b
    
third :: (a,b,c) -> c
third (_,_,c) = c
        
mismatch :: Type -> Type -> Context a
mismatch t1 t2 = lift $ lift $ Err $ Mismatch t1 t2

unbound :: Name -> Context a 
unbound (Name x) = lift $ lift $ Err $ UnboundVar x

nullarith :: Context a 
nullarith = lift $ lift $ Err NullArith

type Error = TCError

data TCError
    = Mismatch Type Type
    | NullArith
    | UnboundVar String
    deriving (Eq)



instance Show TCError where
    show (Mismatch t1 t2) = "Expected " ++ show t1 ++ " got " ++ show t2
    show (UnboundVar x) = "Unbound Variable: " ++ x
    show NullArith = "Airthmatic op with no arguments!"


newenv :: TyEnv 
newenv = []

newgenerator = 0

fresh :: Context Type
fresh = do 
    count <- get
    let count' = count + 1
    put count'
    return $ TyVar $ TName count'

bind :: Name -> Type -> TyEnv -> TyEnv
bind n t e = (n,t):e

bindAll :: [(Name, Type)] -> TyEnv -> TyEnv
bindAll binds env = binds ++ env

findVar :: Name -> Context Type
findVar n = do 
   m :: Maybe Type <- asks $ lookup n 
   case m of 
    Just t -> return t 
    Nothing -> unbound n

