module Prettifier (prettyShow, prettyShowList) where

import Ast

indent count = take (4 * count) (cycle "    ")

class PrettyShow a where
    prettyShow :: a -> Int -> String

prettyShowList :: PrettyShow a => [a] -> Int -> String -> String
prettyShowList [] ic sep = ""
prettyShowList [x] ic sep = prettyShow x ic
prettyShowList (x:xs) ic sep = prettyShow x ic ++ sep ++ prettyShowList xs ic sep

instance PrettyShow TypeId where
    prettyShow (TypeId typeName) ic = typeName

instance PrettyShow VarId where
    prettyShow (VarId varName) ic = varName

instance PrettyShow ProgAST where
    prettyShow (ProgAST types vars) ic = 
        prettyShow types ic ++
        prettyShow vars ic
        
instance PrettyShow CompTypeAST where
    prettyShow (CompSimpleAST typeId) ic = prettyShow typeId ic
    prettyShow (CompListAST compType) ic = "[" ++ prettyShow compType ic ++ "]"
    prettyShow (CompTupleAST compTypeList) ic = "(" ++ prettyShowList compTypeList ic ", " ++ ")"
    prettyShow (CompFuncAST compType1 compType2) ic = "(" ++ prettyShow compType1 ic ++ " -> " ++ prettyShow compType2 ic ++ ")" 
      
instance PrettyShow TypeVarAST where
    prettyShow (UntypedVarAST varId) ic = prettyShow varId ic
    prettyShow (TypedVarAST varId compType) ic = prettyShow varId ic ++ "::" ++ prettyShow compType ic

instance PrettyShow TypeDclAST where
    prettyShow (TypeDclAST typeId cons rest) ic = 
        indent ic ++ "type " ++ prettyShow typeId ic ++ " = {\n" ++ prettyShowList cons (ic + 1) "" ++ 
        indent ic ++ "}\n" ++
        "\n" ++ prettyShow rest ic
    prettyShow (EpsTypeDclAST) ic = ""

instance PrettyShow ConsAST where
    prettyShow (SingleConsAST typeId) ic = 
        indent ic ++ "| " ++ prettyShow typeId ic ++ "\n"
    prettyShow (DoubleConsAST typeId compType) ic =
        indent ic ++ "| " ++ prettyShow typeId ic ++ " " ++ prettyShow compType ic ++ "\n"

instance PrettyShow VarDclAST where
    prettyShow (VarDclAST typeVar expr rest) ic = 
        indent ic ++ "var " ++ prettyShow typeVar ic ++ " = " ++ prettyShow expr ic ++
        "\n" ++ prettyShow rest ic
    prettyShow (EpsVarDclAST) ic = ""

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
    prettyShow (LambdaExprAST varId expr) ic = "(" ++ prettyShow varId ic ++ ") => {" ++ 
        indent ic ++ prettyShow expr (ic + 1) ++
        indent ic ++ "}" 
    --  expr1 expr2
    prettyShow (FunAppExprAST expr1 expr2) ic = "(" ++ prettyShow expr1 ic ++ " " ++ prettyShow expr2 ic ++ ")"
    -- (...)
    prettyShow (TupleExprAST elements) ic = "(" ++ prettyShowList elements ic ", " ++ ")" 
    --  [...]
    prettyShow (ListExprAST elements) ic = "[" ++ prettyShowList elements ic ", " ++ "]"
    --  match {
    --      ...
    --  }
    prettyShow (MatchExprAST expr matchBodies) ic = 
        "\n" ++ indent ic ++ "match " ++ prettyShow expr ic ++ " {\n" ++ 
        prettyShowMatchBodies matchBodies (ic + 1) ++
        indent ic ++ "}\n"
    --  case {
    --      ...
    --  }
    prettyShow (CaseExprAST cases) ic = 
        "\n" ++ indent ic ++ "case {\n" ++
        prettyShowCaseBodies cases (ic + 1) ++
        "\n" ++ indent ic ++ "}\n"
    -- let ... = ... in (
    --      ...
    --  )
    prettyShow (LetInExprAST typeVar expr1 expr2) ic = 
        "\n" ++ indent ic ++ "let " ++ prettyShow typeVar ic ++ " = " ++ prettyShow expr1 ic ++ " in (" ++ 
        prettyShow expr2 (ic + 1) ++ "\n" ++
        indent ic ++ ")"

--  | ... -> ...
prettyShowMatchBodies :: [(PatternAST, ExprAST)] -> Int -> String
prettyShowMatchBodies [] ic = ""
prettyShowMatchBodies ((pattern, expr):xs) ic = 
    indent ic ++ "| " ++ prettyShow pattern ic ++ " -> " ++ prettyShow expr (ic + 1) ++ "\n" ++ prettyShowMatchBodies xs ic

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
    prettyShow (ListPatternAST patterns) ic = "[" ++ prettyShowList patterns ic ", " ++ "]"
    prettyShow (TuplePatternAST patterns) ic = "(" ++ prettyShowList patterns ic ", " ++ ")"
    --  (..:..)
    prettyShow (DecompPatternAST pattern varId) ic = "(" ++ prettyShow pattern ic ++ ":" ++ prettyShow varId ic ++ ")"
    prettyShow (WildPatternAST) ic = "?"

instance PrettyShow ConstAST where
    prettyShow (IntConstAST val) ic = show val
    prettyShow (BoolConstAST val) ic = show val
    prettyShow (FloatConstAST val) ic = show val
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
    prettyShow (OpenReadConstAST) ic = "open_read"
    prettyShow (OpenWriteConstAST) ic = "open_write"
    prettyShow (CloseConstAST) ic = "close"
    prettyShow (ReadConstAST) ic = "read"
    prettyShow (WriteConstAST) ic = "write"
    prettyShow (DeleteConstAST) ic = "delete"