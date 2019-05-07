module Ast
    ( UtilData(..)
    , BonsaiType(..) 
    , TypeId(..)
    , VarId(..)
    , ProgAST(..)
    , CompTypeAST(..)
    , TypeVarAST(..)
    , TypeDclAST(..)
    , ConsAST(..)
    , VarDclAST(..)
    , PredAST(..)
    , ExprAST(..)
    , PatternAST(..)
    , ConstAST(..)
    , Types(..)
    , Prim(..)
    , TermConstructor
    , initUtilData
    , LazyAlgebraicType
    , LazySig
    ) where

import Data.Word (Word8)
import Data.Set as Set
import Data.Map as Map

-- type-environment type
type Env = Map VarId Types

-- type for sets of termconstructors
type Sig = Set TermConstructor

type LazySig = Set LazyAlgebraicType 

type LazyAlgebraicType = (TypeId, [String])

-- a termconstructor has a name an associated type and optionally a signature
type TermConstructor = (TypeId, Types, Types)

data Types = PrimType Prim
           | FuncType Types Types
           | TuplType [Types]
           | ListType Types
           | EmptList
           | AlgeType TypeId
           | AlgePoly TypeId [Types]
           | PolyType String
           | UniqType Types Bool
           | LazyType ExprAST
           | LazyPair ExprAST Types
           | LazyFunc VarId ExprAST Env

data Prim = IntPrim
          | FloatPrim
          | BoolPrim
          | CharPrim
          | FilePrim
          | SystemPrim
          deriving (Eq, Ord) 

instance Eq Types where
    PrimType p1 == PrimType p2 = p1 == p2
    FuncType ta1 tb1 == FuncType ta2 tb2 = ta1 == ta2 && tb1 == tb2
    TuplType typs1 == TuplType typs2 = typs1 == typs2
    ListType typ1 == ListType typ2 = typ1 == typ2
    EmptList == EmptList = True
    ListType _ == EmptList = True
    EmptList == ListType _ = True
    AlgeType typeId1 == AlgePoly typeId2 _ = typeId1 == typeId2
    AlgePoly typeId1 _ == AlgeType typeId2 = typeId1 == typeId2
    AlgeType typeId1 == AlgeType typeId2 = typeId1 == typeId2
    AlgePoly typeId1 polys1 == AlgePoly typeId2 polys2 = typeId1 == typeId2 && polys1 == polys2
    PolyType name1 == PolyType name2 = name1 == name2
    UniqType typ1 valid1 == UniqType typ2 valid2 = typ1 == typ2 && valid1 == valid2
    _ == _ = False

instance Ord Types where
    PrimType p1 `compare` PrimType p2 = p1 `compare` p2
    FuncType ta1 tb1 `compare` FuncType ta2 tb2 = (ta1 `compare` ta2) `compare` (tb1 `compare` tb2)
    TuplType typs1 `compare` TuplType typs2 = typs1 `compare` typs2
    ListType typ1 `compare` ListType typ2 = typ1 `compare` typ2
    AlgeType typeId1 `compare` AlgeType typeId2 = typeId1 `compare` typeId2
    AlgePoly typeId1 _ `compare` AlgePoly typeId2 _ = typeId1 `compare` typeId2
    UniqType typ1 _ `compare` UniqType typ2 _ = typ1 `compare` typ2
    _ `compare` _ = 1 `compare` 1

instance Show Prim where
    show IntPrim    = "Int"
    show FloatPrim  = "Float"
    show BoolPrim   = "Bool"
    show CharPrim   = "Char"
    show FilePrim   = "File"
    show SystemPrim = "System"

instance Show Types where
    show (PrimType prim)      = show prim 
    show (FuncType typ1 typ2) = "(" ++ show typ1 ++ " -> " ++ show typ2 ++ ")"
    show (TuplType typs')     = "(" ++ ([show typ' | typ' <- init typs'] >>= (++ ", ")) ++ show (last typs') ++ ")"
    show (ListType typ)       = "[" ++ show typ ++ "]"
    show EmptList             = "[]"
    show (AlgeType typeId)    = typeName typeId
    show (AlgePoly typeId ps) = typeName typeId ++ "<" ++ ([show typ' | typ' <- init ps] >>= (++ ", ")) ++ show (last ps) ++ ">"
    show (PolyType name)      = name
    show (UniqType typ _)     = show typ ++ "*"
    show _                    = error "cannot convert a lazy type to string" 

type Sort = String

data UtilData = UtilData { 
                            nodeType   :: BonsaiType      -- the AST node's type
                          , position   :: (Int, Int, Int) -- the line and column numbers in the source file as well as indent offset
                          , sourceLine :: String          -- the actual source line
                         } deriving Show

initUtilData :: UtilData
initUtilData = UtilData Untyped (0, 0, 0) ""

data BonsaiType = Untyped
                | Typed Types
                deriving (Show, Eq)

data TypeId = TypeId { typeName :: String } deriving Show
data VarId = VarId { 
                      varName :: String
                    , varType :: BonsaiType                
                   } deriving Show

instance Eq TypeId where
    TypeId s1 == TypeId s2 = s1 == s2 
                
instance Ord TypeId where
    TypeId s1 `compare` TypeId s2 = s1 `compare` s2  

instance Eq VarId where
    VarId s1 _ == VarId s2 _ = s1 == s2 

instance Ord VarId where
    VarId s1 _ `compare` VarId s2 _ = s1 `compare` s2    

data ProgAST = ProgAST TypeDclAST VarDclAST UtilData deriving Show

data CompTypeAST = CompSimpleAST TypeId UtilData
                 | CompSimplePolyAST VarId UtilData
                 | CompPolyAST TypeId [CompTypeAST] UtilData
                 | CompListAST CompTypeAST UtilData
                 | CompTupleAST [CompTypeAST] UtilData
                 | CompFuncAST CompTypeAST CompTypeAST UtilData
                 deriving Show

data TypeVarAST = UntypedVarAST VarId UtilData
                | TypedVarAST VarId CompTypeAST UtilData
                deriving Show

data TypeDclAST = TypeDclAST TypeId [ConsAST] TypeDclAST UtilData
                | TypePolyDclAST TypeId [VarId] [ConsAST] TypeDclAST UtilData
                | EpsTypeDclAST 
                deriving Show

data ConsAST = SingleConsAST TypeId UtilData
             | DoubleConsAST TypeId CompTypeAST UtilData
             deriving Show

data VarDclAST = VarDclAST TypeVarAST ExprAST VarDclAST UtilData
               | EpsVarDclAST
               deriving Show

data PredAST = PredExprAST ExprAST UtilData
             | PredWildAST UtilData
             deriving Show

data ExprAST = VarExprAST VarId UtilData
             | TypeExprAST TypeId UtilData
             | ConstExprAST ConstAST UtilData
             | ParenExprAST ExprAST UtilData
             | LambdaExprAST TypeVarAST ExprAST UtilData
             | FunAppExprAST ExprAST ExprAST UtilData
             | TupleExprAST [ExprAST] UtilData
             | ListExprAST [ExprAST] UtilData
             | MatchExprAST ExprAST [(PatternAST, ExprAST)] UtilData
             | CaseExprAST [(PredAST, ExprAST)] UtilData
             | LetInExprAST TypeVarAST ExprAST ExprAST UtilData
             deriving Show

data PatternAST = ConstPatternAST ConstAST UtilData
                | VarPatternAST VarId UtilData
                | TypePatternAST TypeId UtilData
                | TypeConsPatternAST TypeId PatternAST UtilData
                | ListPatternAST [PatternAST] UtilData
                | TuplePatternAST [PatternAST] UtilData
                | DecompPatternAST PatternAST VarId UtilData
                | WildPatternAST UtilData 
                deriving Show

data ConstAST = IntConstAST Int UtilData
              | BoolConstAST Bool UtilData
              | FloatConstAST Float UtilData
              | CharConstAST Word8 UtilData
              | UnaryMinusConstAST UtilData
              | PlusConstAST UtilData
              | MinusConstAST UtilData
              | TimesConstAST UtilData
              | DivideConstAST UtilData
              | ModuloConstAST UtilData
              | EqualsConstAST UtilData
              | NotConstAST UtilData
              | GreaterConstAST UtilData
              | LessConstAST UtilData
              | GreaterOrEqualConstAST UtilData
              | LessOrEqualConstAST UtilData
              | AppenConstAST UtilData
              | ConcatenateConstAST UtilData
              | AndConstAST UtilData
              | OrConstAST UtilData
              | BiLShiftConstAST UtilData
              | BiRShiftConstAST UtilData
              | BiNotConstAST UtilData
              | BiAndConstAST UtilData
              | BiXorConstAST UtilData
              | BiOrConstAST UtilData
              | OpenReadConstAST UtilData
              | OpenWriteConstAST UtilData
              | CloseConstAST UtilData
              | ReadConstAST UtilData
              | WriteConstAST UtilData
              | DeleteConstAST UtilData
              | ShowConstAST UtilData
              | ToIntConstAST UtilData
              | ToFloatConstAST UtilData
              | IntToCharAST UtilData
              | CharToIntAST UtilData
              deriving Show

instance Eq ConstAST where
    (IntConstAST v1 _) == (IntConstAST v2 _) = v1 == v2
    (BoolConstAST v1 _) == (BoolConstAST v2 _) = v1 == v2
    (FloatConstAST v1 _) == (FloatConstAST v2 _) = v1 == v2
    (CharConstAST v1 _) == (CharConstAST v2 _) = v1 == v2

instance Ord ConstAST where
    (IntConstAST v1 _) `compare` (IntConstAST v2 _) = v1 `compare` v2
    (BoolConstAST v1 _) `compare` (BoolConstAST v2 _) = v1 `compare` v2
    (FloatConstAST v1 _) `compare` (FloatConstAST v2 _) = v1 `compare` v2
    (CharConstAST v1 _) `compare` (CharConstAST v2 _) = v1 `compare` v2    
