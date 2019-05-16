import Ast
import Test.HUnit

testEqualTypes = TestCase $ assertEqual 
    "Should be true" True ((PrimType IntPrim) == (PrimType IntPrim))

testEqualTypesFail = TestCase $ assertEqual
    "Should be false" False ((PrimType IntPrim) == (PrimType FloatPrim))

testOrdTypes = TestCase $ assertEqual
    "Should return LT if first float is less than last float" "LT" (show(compare 1.01 1.02))

testOrdTypesFail = TestCase $ assertEqual
    "Should return false if compare set to EQ but it is not" False (show(compare 2 3) == "EQ")

testShowPrim = TestCase $ assertEqual
    "Should be true if string is the same as prim" "Int" ((show IntPrim))

testShowPrimEmpty = TestCase $ assertEqual
    "Should be false with empty string" False ((show SystemPrim) == "")

testShowTypes = TestCase $ assertEqual
    "Should give certain string" "(Int -> Int)" ((show (FuncType (PrimType IntPrim) (PrimType IntPrim))))

testEqTypeId = TestCase $ assertEqual
    "Should be false if not equal, also with difference with small and big chars" False ((TypeId "TypeName") == (TypeId "typeName"))

testOrdTypeId = TestCase $ assertEqual
    "Should return GT if first typeId is longer" "GT" (show(compare (TypeId "TypeNames") (TypeId "TypeName")))

testEQVarId = TestCase $ assertEqual
    "Should be true as long the types is EQ" 
    True ((VarId "name" (Typed (PrimType IntPrim))) == (VarId "name" (Typed (PrimType IntPrim))))

testEQVarIdfalse = TestCase $ assertEqual
    "Should be false if the VarId isn't the same" 
    False ((VarId "name1" (Typed (PrimType IntPrim))) == (VarId "name2" (Typed (PrimType IntPrim))))

testEQIntConstAST = TestCase $ assertEqual
    "Should be true if its the same value, allthough position is different because of wildcards" 
    True ((IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")) == 
        (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 1, 1) "source")))

testEQIntConstASTFalse = TestCase $ assertEqual
    "Should be false because the value are different, although the position is the same" 
    False ((IntConstAST 2 (UtilData (Typed (PrimType IntPrim)) (1, 1, 1) "source")) == 
        (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 1, 1) "source")))





main = runTestTT $ TestList [testEqualTypes
                            , testEqualTypesFail
                            , testOrdTypes
                            , testOrdTypesFail
                            , testShowPrim
                            , testShowPrimEmpty
                            , testShowTypes
                            , testEqTypeId
                            , testOrdTypeId
                            , testEQVarId
                            , testEQVarIdfalse
                            , testEQIntConstAST
                            , testEQIntConstASTFalse
                            ]