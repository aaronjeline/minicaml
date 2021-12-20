{-# Language DuplicateRecordFields, DeriveFunctor, ScopedTypeVariables, TupleSections, NamedFieldPuns, FlexibleInstances #-}
module TypeConstraints 
(doConstrain, Typing(..), Constraint, allFV)
where
import Lang
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S

type ConstraintResult = (Type, [Constraint])
type Constraint = (Type, Type)
type Context a = ReaderT TyEnv (StateT Generator Typing) a
type TyEnv = [(Name, Type)]
type Generator = Int

allFV :: [Constraint] -> S.Set TypeName
allFV cs = S.unions $ (\(t1,t2) -> freeVars t1 `S.union` freeVars t2) <$> cs


doConstrain :: Exp -> Typing (Type, [Constraint])
doConstrain e = do 
    (r, cs) <- evalStateT (runReaderT (buildConstraints e) newenv) newgenerator
    return (r, cs)

buildConstraints :: Exp -> Context ConstraintResult
buildConstraints U = return (Unit, [])
buildConstraints (Int _) = return (Integer, [])
buildConstraints (Bool _) = return (Boolean, [])
buildConstraints (Str _) = return (String, [])
buildConstraints (Arith o xs) = do
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
buildConstraints (Letrec defs body) = do 
    (binds, cs) <- constrainDefs defs 
    (t, cs') <- local (bindAll binds) $ buildConstraints body
    return (t, cs ++ cs')
buildConstraints (App f args) = do 
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
mismatch t1 t2 = lift $ lift $ Mismatch t1 t2

unbound :: Name -> Context a 
unbound (Name x) = lift $ lift $ UnboundVar x

data Typing a
    = Ok a
    | Mismatch Type Type
    | UnboundVar String
    deriving (Functor)

instance Show a => Show (Typing a) where
    show (Ok a) = show a
    show (Mismatch t1 t2) = "Expected " ++ show t1 ++ " got " ++ show t2
    show (UnboundVar x) = "Unbound Variable: " ++ x

instance Applicative Typing where
    pure = Ok
    Ok f <*> Ok a = Ok $ f a
    Mismatch t1 t2 <*> _ = Mismatch t1 t2
    UnboundVar x <*> _ = UnboundVar x
    _ <*> Mismatch t1 t2 = Mismatch t1 t2 
    _ <*> UnboundVar x = UnboundVar x

instance Monad Typing where
    Ok a >>= f = f a
    Mismatch t1 t2 >>= _ = Mismatch t1 t2
    UnboundVar x >>= _ = UnboundVar x

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

