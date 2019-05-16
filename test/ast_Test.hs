import Ast
import Test.HUnit

testEqualTypes = TestCase $ assertEqual 
    "Should be true" True ((PrimType IntPrim) == (PrimType IntPrim))

testEqualTypesFail = TestCase $ assertEqual
    "Should be false" False ((PrimType IntPrim) == (PrimType FloatPrim))

testShowPrim = TestCase $ assertEqual
    "Should be true if string is the same as prim" "Int" ((show IntPrim))

testShowPrimEmpty = TestCase $ assertEqual
    "Should be false with empty string" False ((show SystemPrim) == "")

testShowTypes = TestCase $ assertEqual
    "Should give certain string" "(Int -> Int)" ((show (FuncType (PrimType IntPrim) (PrimType IntPrim))))

testEqTypeId = TestCase $ assertEqual
    "Should be true if equals" False ((TypeId "TypeName") == (TypeId "typeName"))



main = runTestTT $ TestList [testEqualTypes
                            , testEqualTypesFail
                            , testShowPrim
                            , testShowPrimEmpty
                            , testShowTypes
                            , testEqTypeId
                            ]