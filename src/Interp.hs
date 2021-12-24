{-# Language DuplicateRecordFields, NamedFieldPuns, DeriveFunctor, ScopedTypeVariables #-}
module Interp 
(interp, Result(..), Error(..), Value(..))
where
import Prelude hiding (error)
import Lang
import Typecheck
import Vector
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Result
import qualified Data.Set as S

type Context a = ReaderT Frame (StateT InterpState (Result Error)) a

interp :: CheckedExp -> Result Error Value
interp ce = evalStateT (runReaderT (interpCheck ce) []) freshState

interpInContext :: CheckedExp -> Frame -> Result Error Value
interpInContext ce ctx = evalStateT (runReaderT (interpCheck ce) ctx) freshState


data Error 
    = ArrayBoundsFailure
    | TypeError
    | UnboundVar Name
    | ReferenceError
    | IdxError
    deriving (Show, Eq, Ord)

data Value 
    = VU
    | VI Int
    | VB Bool
    | VS String
    | TV [Value]
    | Func Closure
    | Vector Reference
    deriving Show

data Closure = Closure { body :: Exp, params :: [Name], env :: Frame }

instance Show Closure where
    show _  = "<closure>"

instance Show Reference where
    show _ = "<vector>"


interpCheck :: CheckedExp -> Context Value 
interpCheck ce = interpStep $ getexp ce 

interpStep :: Exp -> Context Value
interpStep U = return VU
interpStep (Int i) = return $ VI i
interpStep (Bool b) = return $ VB b
interpStep (Str s) = return $ VS s
interpStep (Arith o es) = do 
    ns <- forM es interpStep >>= mapM getNum
    applyOp o ns
interpStep (If e1 e2 e3) = do 
    v1 <- interpStep e1 >>= getBool
    if v1 then 
        interpStep e2
    else 
        interpStep e3
interpStep (Variable x) = do 
    find x 
interpStep (Let n e1 e2) = do 
    v1 <- interpStep e1 
    local (bind n v1) (interpStep e2)
interpStep l@(Lam params body) = do
    let free = S.toList $ freeVars l
    vals <- forM free find
    let env = zip free vals
    return (Func $ Closure { params, env, body }) 
interpStep (Letrec defs body) = do
    closures <- forM defs createClosure
    let names = name <$> defs
    let env = zip names closures
    local (bindAll env) (interpStep body)
interpStep (f :@ es) = do 
    fv <- interpStep f >>= getClosure
    args <- forM es interpStep
    apply fv args
interpStep (MakeTuple es) = do
    vs <- forM es interpStep
    return $ TV vs
interpStep BindTuple { names, tuple, body } = do
    vs :: [Value] <- interpStep tuple >>= getTuple
    if length vs /= length names then
        error TypeError
    else 
        local (const $ zip names vs) (interpStep body)
interpStep MakeArray { count, init } = do 
    countV <- interpStep count >>= getNum
    initV <- interpStep init
    newArray countV initV 
interpStep ArrayRead { idx, arr } = do 
    idxV <- interpStep idx >>= getNum
    r <- interpStep arr >>= getRef
    readArray idxV r
interpStep ArrayWrite { idx, arr, val } = do     
    idxV <- interpStep idx >>= getNum
    r <- interpStep arr >>= getRef
    v <- interpStep val
    writeArray idxV v r
    return VU
    

apply :: Closure -> [Value] -> Context Value
apply Closure { body, params, env } args = do 
    let env' = zip params args ++ env
    local (const env') (interpStep body)


createClosure :: Def -> Context Value 
createClosure Def { name, params, body } = do 
    let capt = S.toList $ freeVarsDef (Def { name, params, body })
    vals <- forM capt find
    let binds = zip capt vals
    return $ fix (\cls -> Func $ Closure { body, params, env = (name,cls) : binds })

bind :: Name -> Value -> Frame -> Frame
bind n v f = (n,v):f

bindAll :: [(Name, Value)] -> Frame -> Frame
bindAll binds frame = binds ++ frame

find :: Name -> Context Value
find x = do 
    env <- asks $ lookup x 
    case env of 
        Just x -> return x
        Nothing -> error $ UnboundVar x

applyOp :: Op -> [Int] -> Context Value
applyOp Add ns = return $ VI $ sum ns
applyOp Mult ns = return $ VI $ product ns
applyOp Eq ns = return $ VB $ all (== f) ns
    where f = head ns

getNum :: Value -> Context Int
getNum (VI i) = return i
getNum _ = error TypeError

getBool :: Value -> Context Bool
getBool (VB b) = return b
getBool _ = error TypeError

getClosure :: Value -> Context Closure
getClosure (Func c) = return c
getClosure _ = error TypeError

getTuple :: Value -> Context [Value]
getTuple (TV vs) = return vs
getTuple _ = error TypeError

getRef :: Value -> Context Reference
getRef (Vector r) = return r
getRef _ = error TypeError

error :: Error ->  Context a
error e = lift $ lift $ Err e



newtype Reference = Ref Int
    deriving Eq

    
type Frame = [(Name, Value)]

data InterpState = IS 
    { vectors :: [(Reference, Vector Value)]
    , nextRef :: Reference }


freshState :: InterpState
freshState = IS { vectors = [], nextRef = Ref 0 }

newArray :: Int -> Value -> Context Value
newArray len init = do 
    ref <- fresh
    let v = makeVector len init
    bindArray ref v
    return $ Vector ref

readArray :: Int -> Reference -> Context Value
readArray idx r = do 
    v <- findArray r
    vecRead idx v `orElse` error IdxError 

writeArray :: Int -> Value -> Reference -> Context ()
writeArray idx val r = do 
    v <- findArray r 
    v' <- vecWrite idx v val `orElse` error IdxError
    replaceArray r v'
    
replaceArray :: Reference -> Vector Value -> Context ()
replaceArray r v = modify (replace r v) 
    where
        replace :: Reference -> Vector Value -> InterpState -> InterpState
        replace r v is = is { vectors = replaceVec r v (vectors is) }
        replaceVec r v ((r',v'):vs) = if r' == r then (r,v):vs else (r',v'):replaceVec r v vs
        replaceVec _ _ [] = undefined

findArray :: Reference -> Context (Vector Value)
findArray r = do 
    vecs <- gets vectors
    lookup r vecs `orElse` error ReferenceError



bindArray :: Reference -> Vector Value -> Context ()
bindArray r v = modify (updateVecs r v)
    
updateVecs :: Reference -> Vector Value -> InterpState -> InterpState
updateVecs r v is = is { vectors = (r,v) : vectors is }


updateRef :: Reference -> InterpState -> InterpState
updateRef r is = is { nextRef = r }

fresh :: Context Reference
fresh = do 
    (Ref i) <- gets nextRef
    modify $ updateRef $ Ref (i + 1)
    return (Ref i)



