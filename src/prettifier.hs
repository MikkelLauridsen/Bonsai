module Prettifier (prettyShow, prettyShowList) where

import Ast

-- Creates 4 * count spaces
indent :: Int -> String
indent count = take (4 * count) (cycle "    ")

class PrettyShow a where
    prettyShow :: a -> Int -> String

-- Calls prettyShow on each element in a list, seperated by 'sep'
prettyShowList :: PrettyShow a => [a] -> Int -> String -> String
prettyShowList [] ic sep = ""
prettyShowList [x] ic sep = prettyShow x ic
prettyShowList (x:xs) ic sep = prettyShow x ic ++ sep ++ prettyShowList xs ic sep

instance PrettyShow TypeId where
    prettyShow (TypeId typeName) ic = typeName

instance PrettyShow VarId where
    prettyShow (VarId varName) ic = varName

instance PrettyShow ProgAST where
    prettyShow (ProgAST types vars _) ic = 
        prettyShow types ic ++
        prettyShow vars ic
        
instance PrettyShow CompTypeAST where
    prettyShow (CompSimpleAST typeId _) ic = prettyShow typeId ic
    prettyShow (CompListAST compType _) ic = "[" ++ prettyShow compType ic ++ "]"
    prettyShow (CompTupleAST compTypeList _) ic = "(" ++ prettyShowList compTypeList ic ", " ++ ")"
    prettyShow (CompFuncAST compType1 compType2 _) ic = "(" ++ prettyShow compType1 ic ++ " -> " ++ prettyShow compType2 ic ++ ")" 
      
instance PrettyShow TypeVarAST where
    prettyShow (UntypedVarAST varId _) ic = prettyShow varId ic
    prettyShow (TypedVarAST varId compType _) ic = prettyShow varId ic ++ "::" ++ prettyShow compType ic

--type ... = { ... }
instance PrettyShow TypeDclAST where
    prettyShow (TypeDclAST typeId cons rest _) ic = 
        indent ic ++ "type " ++ prettyShow typeId ic ++ " = {\n" ++ prettyShowList cons (ic + 1) "" ++ 
        indent ic ++ "}\n" ++
        "\n" ++ prettyShow rest ic
    prettyShow (EpsTypeDclAST) ic = ""

instance PrettyShow ConsAST where
    prettyShow (SingleConsAST typeId _) ic = 
        indent ic ++ "| " ++ prettyShow typeId ic ++ "\n"
    prettyShow (DoubleConsAST typeId compType _) ic =
        indent ic ++ "| " ++ prettyShow typeId ic ++ " " ++ prettyShow compType ic ++ "\n"

--var ... = ...
instance PrettyShow VarDclAST where
    prettyShow (VarDclAST typeVar expr rest _) ic = 
        indent ic ++ "var " ++ prettyShow typeVar ic ++ " = " ++ prettyShow expr ic ++
        "\n" ++ prettyShow rest ic
    prettyShow (EpsVarDclAST) ic = ""

instance PrettyShow PredAST where
    prettyShow (PredExprAST expr _) ic = prettyShow expr ic
    prettyShow (PredWildAST _) ic = "?"


instance PrettyShow ExprAST where
    prettyShow (VarExprAST varId _) ic = prettyShow varId ic
    prettyShow (TypeExprAST typeId _) ic = prettyShow typeId ic
    prettyShow (ConstExprAST const' _) ic = prettyShow const' ic
    --  (...)
    prettyShow (ParenExprAST expr _) ic = "(" ++ prettyShow expr ic ++ ")"
    --  (...) => {...}
    prettyShow (LambdaExprAST var expr _) ic = "(" ++ prettyShow var ic ++ ") => {" ++ 
        indent ic ++ prettyShow expr (ic + 1) ++
        indent ic ++ "}" 
    --  expr1 expr2
    prettyShow (FunAppExprAST expr1 expr2 _) ic = "(" ++ prettyShow expr1 ic ++ " " ++ prettyShow expr2 ic ++ ")"
    -- (...)
    prettyShow (TupleExprAST elements _) ic = "(" ++ prettyShowList elements ic ", " ++ ")" 
    --  [...]
    prettyShow (ListExprAST elements _) ic = "[" ++ prettyShowList elements ic ", " ++ "]"
    --  match {
    --      ...
    --  }
    prettyShow (MatchExprAST expr matchBodies _) ic = 
        "\n" ++ indent ic ++ "match " ++ prettyShow expr ic ++ " {\n" ++ 
        prettyShowMatchBodies matchBodies (ic + 1) ++
        indent ic ++ "}\n"
    --  case {
    --      ...
    --  }
    prettyShow (CaseExprAST cases _) ic = 
        "\n" ++ indent ic ++ "case {\n" ++
        prettyShowCaseBodies cases (ic + 1) ++
        "\n" ++ indent ic ++ "}\n"
    -- let ... = ... in (
    --      ...
    --  )
    prettyShow (LetInExprAST typeVar expr1 expr2 _) ic = 
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
    prettyShow (ConstPatternAST const' _) ic = prettyShow const' ic
    prettyShow (VarPatternAST varId _) ic = prettyShow varId ic
    prettyShow (TypePatternAST typeId _) ic = prettyShow typeId ic
    prettyShow (TypeConsPatternAST typeId pattern _) ic = prettyShow typeId ic ++ " " ++ prettyShow pattern ic
    -- [...]
    prettyShow (ListPatternAST patterns _) ic = "[" ++ prettyShowList patterns ic ", " ++ "]"
    -- (...)
    prettyShow (TuplePatternAST patterns _) ic = "(" ++ prettyShowList patterns ic ", " ++ ")"
    -- (..:..)
    prettyShow (DecompPatternAST pattern varId _) ic = "(" ++ prettyShow pattern ic ++ ":" ++ prettyShow varId ic ++ ")"
    prettyShow (WildPatternAST _) ic = "?"

instance PrettyShow ConstAST where
    prettyShow (IntConstAST val _) ic = show val
    prettyShow (BoolConstAST val _) ic = show val
    prettyShow (FloatConstAST val _) ic = show val
    prettyShow (CharConstAST val _) ic = show val
    prettyShow (UnaryMinusConstAST _) ic = "~"
    prettyShow (PlusConstAST _) ic = "+"
    prettyShow (MinusConstAST _) ic = "-"
    prettyShow (TimesConstAST _) ic = "*"
    prettyShow (DivideConstAST _) ic = "/"
    prettyShow (ModuloConstAST _) ic = "%"
    prettyShow (EqualsConstAST _) ic = "=="
    prettyShow (NotConstAST _) ic = "!"
    prettyShow (GreaterConstAST _) ic = ">"
    prettyShow (LessConstAST _) ic = "<"
    prettyShow (GreaterOrEqualConstAST _) ic = ">="
    prettyShow (LessOrEqualConstAST _) ic = "<="
    prettyShow (AppenConstAST _) ic = ":"
    prettyShow (ConcatenateConstAST _) ic = "++"
    prettyShow (AndConstAST _) ic = "&&"
    prettyShow (OrConstAST _) ic = "||"
    prettyShow (BiLShiftConstAST _) ic = "b<<"
    prettyShow (BiRShiftConstAST _) ic = "b>>"
    prettyShow (BiNotConstAST _) ic = "b~"
    prettyShow (BiAndConstAST _) ic = "b&"
    prettyShow (BiXorConstAST _) ic = "b^"
    prettyShow (BiOrConstAST _) ic = "b|"
    prettyShow (OpenReadConstAST _) ic = "open_read"
    prettyShow (OpenWriteConstAST _) ic = "open_write"
    prettyShow (CloseConstAST _) ic = "close"
    prettyShow (ReadConstAST _) ic = "read"
    prettyShow (WriteConstAST _) ic = "write"
    prettyShow (DeleteConstAST _) ic = "delete"
    prettyShow (ShowConstAST _) ic = "show"
    prettyShow (ToIntConstAST _) ic = "s2i"
    prettyShow (ToFloatConstAST _) ic = "s2f"
    prettyShow (IntToCharAST _) ic = "i2c"
    prettyShow (CharToIntAST _) ic = "c2i"