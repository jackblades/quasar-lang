
module Ordo where

import Data.Map as M
import Data.Set as S

data Ref a

type Name = String
type Id = Int
type Level = Int

data Ty
    = TConst Name
    | TBool
    | TInt
    | TFloat
    | TString
    | TList Ty
    | TApp [(Ty, Ty)]
    | TArrow Ty Ty
    | TyVar -- (Ref TVar)
    | TRecord Row
    | TVariant Row
    | TRowEmpty
    | TRowExtend Name Ty Row
    deriving (Eq, Show)

type Row = Ty
type Constraints = Set Name

data Tvar
    = Unbound Id Level
    | UnboundRow Id Level Constraints
    | Link Ty
    | Generic Id
    | GenericRow Id Constraints
    deriving (Eq, Show)

data BinOp
    = Plus
    | Minus
    | Multiply
    | Divide
    | And
    | Or
    | Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Lesser
    | LesserEqual
    deriving (Eq, Show)

data UnOp = Negative deriving (Eq, Show)

data Expr
    = EBool Bool
    | EInt Int
    | EFloat Float
    | EString String
    | EVar Name
    | ECall Expr Expr
    | EFun Pattern Expr
    | ELet Pattern Expr Expr
    | ERecordSelect Expr Name
    | ERecordExtend Name Expr Expr
    | ERecordRestrict Expr Name
    | ERecordEmpty
    | EVariant Name Expr
    | ECase Expr [Maybe (Pattern, Expr, Guard)] (Maybe (Name, Expr))
    | EIfThenElse Expr Expr Expr
    | EBinOp Expr BinOp Expr
    | EUnOp UnOp Expr
    | EFix Name
    | EListEmpty
    | EListCons Expr Expr
    | EOpen String
    | EType Expr Ty
    deriving (Eq, Show)

type Pattern = Expr
type Guard = Expr

data Value
    = VBool Bool
    | VInt Int
    | VFloat Float
    | VString String
    | VFun (Map Name Value) Pattern Expr
    | VRecord (Map Name Value)
    | VVariant Name Value
    | VList [Value]
    deriving (Eq, Show)

data Entry
    = Entry { name :: Name, constraints :: Set Name }
    deriving (Eq, Show)
