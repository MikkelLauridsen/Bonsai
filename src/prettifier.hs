module Prettifier (prettyShow) where

import Ast

indent count = take (4 * count) (cycle "    ")

class PrettyShow a where
    prettyShow :: a -> Int -> String

instance PrettyShow TypeId where
    prettyShow (TypeId typeName) ic = typeName

instance PrettyShow VarId where
    prettyShow (VarId varName) ic = varName

instance PrettyShow ProgAST where
    prettyShow (ProgAST types vars) ic = 
        "#Type declarations:\n" ++              --Kan fjernes
        prettyShowTypeDcls types ic ++ "\n" ++ 
        "#Variable declarations:\n" ++          --Kan fjernes
        prettyShowVarDcl vars ic ++ "\n"
        
instance PrettyShow CompTypeAST where
    prettyShow (CompSimpleAST typeId) ic = prettyShow typeId ic
    prettyShow (CompListAST compType) ic = "[" ++ prettyShow compType ic ++ "]"
    prettyShow (CompTupleAST compTypeList) ic = "(" ++ prettyShowCompTypes compTypeList ic ++ ")"
    prettyShow (CompFuncAST compType1 compType2) ic = "(" ++ prettyShow compType1 ic ++ " -> " ++ prettyShow compType2 ic ++ ")" 

prettyShowCompTypes :: [CompTypeAST] -> Int -> String
prettyShowCompTypes [] ic = ""
prettyShowCompTypes [x] ic = prettyShow x ic
prettyShowCompTypes (x:xs) ic = prettyShow x ic ++ ", " ++ prettyShowCompTypes xs ic
      
instance PrettyShow TypeVarAST where
    prettyShow (UntypedVarAST varId) ic = prettyShow varId ic
    prettyShow (TypedVarAST varId compType) ic = prettyShow varId ic ++ "::" ++ prettyShow compType ic

prettyShowTypeDcls :: [TypeDclAST] -> Int -> String
prettyShowTypeDcls [] ic = ""
prettyShowTypeDcls ((TypeDclAST typeId cons):xs) ic = 
    indent ic ++ "type " ++ prettyShow typeId ic ++ " = {\n" ++ prettyShowCons cons (ic + 1) ++ "\n\n" ++
    indent ic ++ "}\n" ++ prettyShowTypeDcls xs ic
    

instance PrettyShow ConsAST where
    prettyShow (SingleConsAST typeId) ic = 
        indent ic ++ "| " ++ prettyShow typeId ic
    prettyShow (DoubleConsAST typeId compType) ic =
        indent ic ++ "| " ++ prettyShow typeId ic ++ " " ++ prettyShow compType ic
 
prettyShowCons :: [ConsAST] -> Int -> String
prettyShowCons [] ic = ""
prettyShowCons (x:xs) ic = prettyShow x ic ++ prettyShowCons xs ic

instance PrettyShow VarDclAST where
    prettyShow (VarDclAST typeId cons) ic = 
        indent ic ++ "type " ++ prettyShow typeId ic ++ " = {\n" ++ 
        prettyShow cons (ic + 1) ++ "\n\n" ++
        indent ic ++ "}\n"

prettyShowVarDcl :: [VarDclAST] -> Int -> String
prettyShowVarDcl [] ic = ""
prettyShowVarDcl ((VarDclAST typeVar expr):xs) ic = 
    indent ic ++ "var " ++ prettyShow typeVar ic ++ " = " ++ prettyShow expr ic ++ "\n\n" ++ 
    prettyShowVarDcl xs ic
        

instance PrettyShow PredAST where
    prettyShow (PredExprAST expr) ic = prettyShow expr ic
    prettyShow (PredWildAST) ic = "?"


instance PrettyShow ExprAST where
    prettyShow (VarExprAST varId) ic = prettyShow varId ic
    prettyShow (TypeExprAST typeId) ic = prettyShow typeId ic
    prettyShow (ConstExprAST const') ic = prettyShow const' ic
    --  (...)
    prettyShow (ParenExprAST expr) ic = "(" ++ prettyShow expr ic ++ ")"
    --  (...) => {...}
    prettyShow (LambdaExprAST typeVar expr) ic = "(" ++ prettyShow typeVar ic ++ ") => {\n" ++ 
        indent ic ++ prettyShow expr (ic + 1) ++
        indent ic ++ "\n}" 
    --  expr1 expr2
    prettyShow (FunAppExprAST expr1 expr2) ic = "(" ++ prettyShow expr1 ic ++ " " ++ prettyShow expr2 ic ++ ")"
    -- (...)
    prettyShow (TupleExprAST elements) ic = "(" ++ prettyShowExprs elements ic ++ ")" 
    --  [...]
    prettyShow (ListExprAST elements) ic = "[" ++ prettyShowExprs elements ic ++ "]"
    --  match {
    --      ...
    --  }
    prettyShow (MatchExprAST expr matchBodies) ic = 
        indent ic ++ "match " ++ prettyShow expr ic ++ " {\n" ++ 
        prettyShowMatchBodies matchBodies (ic + 1) ++
        indent ic ++ "}"
    --  case {
    --      ...
    --  }
    prettyShow (CaseExprAST cases) ic = 
        "\n" ++ indent ic ++ "case {\n" ++
        prettyShowCaseBodies cases (ic + 1) ++
        indent ic ++ "}"
    -- let ... = ... in (
    --      ...
    --  )
    prettyShow (LetInExprAST typeVar expr1 expr2) ic = 
        "\n" ++ indent ic ++ "let " ++ prettyShow typeVar ic ++ " = " ++ prettyShow expr1 ic ++ " in (\n" ++ 
        prettyShow expr2 (ic + 1) ++ "\n" ++
        indent ic ++ ")"
 
--  expr, expr, ...
prettyShowExprs :: [ExprAST] -> Int -> String
prettyShowExprs [] ic = ""
prettyShowExprs [x] ic = prettyShow x ic
prettyShowExprs (x:xs) ic = prettyShow x ic ++ ", " ++ prettyShowExprs xs ic

--  | ... -> ...
prettyShowMatchBodies :: [(PatternAST, ExprAST)] -> Int -> String
prettyShowMatchBodies [] ic = ""
prettyShowMatchBodies ((pattern, expr):xs) ic = 
    indent ic ++ "| " ++ prettyShow pattern ic ++ " -> " ++ prettyShow expr ic ++ "\n" ++ prettyShowMatchBodies xs ic


--  | ... -> ...
prettyShowCaseBodies :: [(PredAST, ExprAST)] -> Int -> String
prettyShowCaseBodies [] ic = ""
prettyShowCaseBodies ((pred, expr):xs) ic = 
    indent ic ++ "| " ++ prettyShow pred ic ++ " -> " ++ prettyShow expr ic ++ "\n"  ++ prettyShowCaseBodies xs ic

instance PrettyShow PatternAST where
    prettyShow (ConstPatternAST const') ic = prettyShow const' ic
    prettyShow (VarPatternAST varId) ic = prettyShow varId ic
    prettyShow (TypePatternAST typeId) ic = prettyShow typeId ic
    prettyShow (TypeConsPatternAST typeId pattern) ic = prettyShow typeId ic ++ " " ++ prettyShow pattern ic
    prettyShow (ListPatternAST patterns) ic = "[" ++ prettyShowPatterns patterns ic ++ "]"
    prettyShow (TuplePatternAST patterns) ic = "(" ++ prettyShowPatterns patterns ic ++ ")"
    --  (..:..)
    prettyShow (DecompPatternAST pattern varId) ic = "(" ++ prettyShow pattern ic ++ ":" ++ prettyShow varId ic ++ ")"
    prettyShow (WildPatternAST) ic = "?"
    
prettyShowPatterns :: [PatternAST] -> Int -> String
prettyShowPatterns [] ic = ""
prettyShowPatterns [x] ic = prettyShow x ic
prettyShowPatterns (x:xs) ic = prettyShow x ic ++ ", " ++ prettyShowPatterns xs ic

instance PrettyShow ConstAST where
    prettyShow (IntConstAST val) ic = show val
    prettyShow (BoolConstAST val) ic = show val
    prettyShow (FloatConstAST val) ic = show val
    prettyShow (StringConstAST val) ic = show val
    prettyShow (CharConstAST val) ic = show val
    prettyShow (UnaryMinusConstAST) ic = "~"
    prettyShow (PlusConstAST) ic = "+"
    prettyShow (MinusConstAST) ic = "-"
    prettyShow (TimesConstAST) ic = "*"
    prettyShow (DivideConstAST) ic = "/"
    prettyShow (ModuloConstAST) ic = "%"
    prettyShow (EqualsConstAST) ic = "="
    prettyShow (NotConstAST) ic = "!"
    prettyShow (GreaterConstAST) ic = ">"
    prettyShow (LessConstAST) ic = "<"
    prettyShow (GreaterOrEqualConstAST) ic = ">="
    prettyShow (LessOrEqualConstAST) ic = "<="
    prettyShow (AppenConstAST) ic = ":"
    prettyShow (ConcatenateConstAST) ic = "++"
    prettyShow (AndConstAST) ic = "&&"
    prettyShow (OrConstAST) ic = "||"