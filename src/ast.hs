module Ast
    ( TypeId(..)
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
    ) where

data TypeId = TypeId { typeName::String } deriving (Show, Eq, Ord)
data VarId = VarId { varName::String } deriving (Show, Eq, Ord)

data ProgAST = ProgAST [TypeDclAST] [VarDclAST] deriving Show

data CompTypeAST = CompSimpleAST TypeId
                 | CompListAST CompTypeAST
                 | CompTupleAST [CompTypeAST]
                 | CompFuncAST CompTypeAST CompTypeAST
                 deriving Show

data TypeVarAST = UntypedVarAST VarId
                | TypedVarAST VarId CompTypeAST
                deriving Show

data TypeDclAST = TypeDclAST TypeId [ConsAST] deriving Show

data ConsAST = SingleConsAST TypeId
             | DoubleConsAST TypeId CompTypeAST
             deriving Show

data VarDclAST = VarDclAST TypeVarAST ExprAST deriving Show

data PredAST = PredExprAST ExprAST
             | PredWildAST
             deriving Show

data ExprAST = VarExprAST VarId
             | TypeExprAST TypeId
             | ConstExprAST ConstAST
             | ParenExprAST ExprAST
             | LambdaExprAST TypeVarAST ExprAST
             | FunAppExprAST ExprAST ExprAST
             | TupleExprAST [ExprAST]
             | ListExprAST [ExprAST]
             | MatchExprAST ExprAST [(PatternAST, ExprAST)]
             | CaseExprAST [(PredAST, ExprAST)]
             | LetInExprAST TypeVarAST ExprAST ExprAST
             deriving Show

data PatternAST = ConstPatternAST ConstAST
                | VarPatternAST VarId
                | TypePatternAST TypeId
                | TypeConsPatternAST TypeId PatternAST
                | ListPatternAST [PatternAST]
                | TuplePatternAST [PatternAST]
                | DecompPatternAST PatternAST VarId
                | WildPatternAST 
                deriving Show

data ConstAST = IntConstAST Int
              | BoolConstAST Bool
              | FloatConstAST Float
              | StringConstAST String
              | CharConstAST Char
              | UnaryMinusConstAST
              | PlusConstAST
              | MinusConstAST
              | TimesConstAST
              | DivideConstAST
              | ModuloConstAST
              | EqualsConstAST
              | NotConstAST
              | GreaterConstAST
              | LessConstAST
              | GreaterOrEqualConstAST
              | LessOrEqualConstAST
              | AppenConstAST
              | ConcatenateConstAST
              | AndConstAST
              | OrConstAST
              deriving Show