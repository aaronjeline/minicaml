{-# Language DuplicateRecordFields, OverloadedStrings, NamedFieldPuns #-}
module Lang where
import qualified Data.Set as S
import Data.String

newtype Name = Name String
    deriving (Eq,Ord)

instance IsString Name where
    fromString = Name

ex :: Name
ex = "hi"

instance Show Name where
    show (Name x) = show x

newtype TypeName = TName Int
    deriving (Eq, Ord)

instance Show TypeName where
    show (TName x) = show x

data Type
    = Integer
    | Boolean
    | String
    | Unit
    | Function [Type] 
    | Tuple [Type]
    | Array Type
    | TyVar TypeName
    deriving (Show,Eq)

freeTVars :: Type -> S.Set TypeName
freeTVars Integer = S.empty
freeTVars Boolean = S.empty
freeTVars Unit = S.empty
freeTVars String = S.empty
freeTVars (TyVar n) = S.singleton n
freeTVars (Function ts) = S.unions $ map freeTVars ts
freeTVars (Tuple ts) = S.unions $ map freeTVars ts
freeTVars (Array t) = freeTVars t

data Exp
    = Int Int
    | Bool Bool
    | Str String
    | U
    | Arith Op [Exp]
    | If Exp Exp Exp 
    | Variable Name
    | Let Name Exp Exp 
    | Lam [Name] Exp
    | Letrec [Def] Exp
    | Exp :@ [Exp]
    | MakeTuple [Exp]
    | BindTuple { names :: [Name], tuple :: Exp, body :: Exp }
    | MakeArray { count :: Exp, init :: Exp }
    | ArrayRead { arr :: Exp, idx :: Exp}
    | ArrayWrite { arr :: Exp, idx :: Exp, val :: Exp }
    deriving (Show,Eq)
    
data Def = Def 
    { name :: Name
    , params :: [Name]
    , body :: Exp }
    deriving (Show,Eq)

freeVars :: Exp -> S.Set Name
freeVars U = S.empty
freeVars (Int _) = S.empty
freeVars (Bool _) = S.empty
freeVars (Str _) = S.empty
freeVars (Arith _ es) = S.unions $ map freeVars es
freeVars (Variable n) = S.singleton n
freeVars (If e1 e2 e3) = freeVars e1 `S.union` freeVars e2 `S.union` freeVars e3
freeVars (Let n e1 e2) = freeVars e1 `S.union` (freeVars e2 `S.difference` S.singleton n)
freeVars (Lam params body) = freeVars body `S.difference` S.fromList params
freeVars (Letrec defs body) = bodyFree `S.union` defFree
    where
        bodyFree = freeVars body `S.difference` bound
        defFree = S.unions $ map freeVarsDef defs
        bound = S.fromList $ map name defs
freeVars (e :@ es) = S.unions $ map freeVars $ e : es
freeVars (MakeTuple es) = S.unions $ map freeVars es
freeVars BindTuple { names, tuple, body } = freeVars tuple `S.union` bodyFree
    where bodyFree = freeVars body `S.difference` S.fromList names
freeVars MakeArray { count, init } = freeVars count `S.union` freeVars init
freeVars ArrayRead { arr, idx } = freeVars arr `S.union` freeVars idx
freeVars ArrayWrite { arr, idx, val } = freeVars arr `S.union` freeVars idx `S.union` freeVars val


freeVarsDef :: Def -> S.Set Name
freeVarsDef Def { name, params, body } = freeVars body `S.difference` bound
    where bound = S.fromList $ name : params


data Op = Add | Mult | Eq deriving (Show,Eq)

