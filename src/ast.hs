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
    , TermConstructor(..)
    , initUtilData
    , AlgebraicType
    ) where

import Data.Word (Word8)
import Data.Set as Set

-- type for sets of algebraic types
type Sig = Set AlgebraicType

type AlgebraicType = (TypeId, [TermConstructor])
-- a termconstructor has a name and optionally a signature
data TermConstructor = ConstConstructor TypeId
                     | CompConstructor  TypeId Types 

data Types = PrimType Prim
           | FuncType Types Types
           | TuplType [Types]
           | ListType Types
           | AlgeType TypeId Sig
           | AlgePoly TypeId [Types] Sig
           | PolyType String
           | UniqType Types Bool
           | OkayType
           | LazyType ExprAST
           | LazyPair ExprAST Types

data Prim = IntPrim
          | FloatPrim
          | BoolPrim
          | CharPrim
          | FilePrim
          | SystemPrim
          deriving Eq 

instance Eq Types where
    PrimType p1 == PrimType p2 = p1 == p2
    FuncType ta1 tb1 == FuncType ta2 tb2 = ta1 == ta2 && tb1 == tb2
    TuplType typs1 == TuplType typs2 = typs1 == typs2
    ListType typ1 == ListType typ2 = typ1 == typ2
    AlgeType typeId1 _ == AlgeType typeId2 _ = typeId1 == typeId2
    AlgePoly typeId1 _ _ == AlgePoly typeId2 _ _ = typeId1 == typeId2
    PolyType name1 == PolyType name2 = name1 == name2
    UniqType typ1 valid1 == UniqType typ2 valid2 = typ1 == typ2 && valid1 == valid2
    OkayType == OkayType = True
    _ == _ = False

instance Show Prim where
    show IntPrim    = "Int"
    show FloatPrim  = "Float"
    show BoolPrim   = "Bool"
    show CharPrim   = "Char"
    show FilePrim   = "File"
    show SystemPrim = "System"

instance Show Types where
    show (PrimType prim)        = show prim 
    show (FuncType typ1 typ2)   = "(" ++ show typ1 ++ "->" ++ show typ2 ++ ")"
    show (TuplType typs')       = "(" ++ ([show typ' | typ' <- typs'] >>= (++ ", ")) ++ ")"
    show (ListType typ)         = "[" ++ show typ ++ "]"
    show (AlgeType typeId _)    = typeName typeId
    show (AlgePoly typeId ps _) = typeName typeId ++ "<" ++ ([show typ' | typ' <- ps] >>= (++ ", ")) ++ ">"
    show (PolyType name)        = name
    show (UniqType typ _)       = show typ ++ "*"
    show _                      = error "cannot convert a lazy type to string" 

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
