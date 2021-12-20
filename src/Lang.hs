{-# Language DuplicateRecordFields, OverloadedStrings #-}
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

freeVars :: Type -> S.Set TypeName
freeVars Integer = S.empty
freeVars Boolean = S.empty
freeVars Unit = S.empty
freeVars String = S.empty
freeVars (TyVar n) = S.singleton n
freeVars (Function ts) = S.unions $ map freeVars ts
freeVars (Tuple ts) = S.unions $ map freeVars ts
freeVars (Array t) = freeVars t

data Exp
    = Int Int
    | Bool Bool
    | Str String
    | U
    | Arith Op [Exp]
    | If Exp Exp Exp 
    | Variable Name
    | Let Name Exp Exp 
    | Letrec [Def] Exp
    | App Exp [Exp]
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

data Op = Add | Mult | Eq deriving (Show,Eq)

