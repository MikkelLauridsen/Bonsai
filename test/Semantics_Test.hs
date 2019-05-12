import Semantics
import Test.HUnit
import Ast
import Data.Set as Set

utilDataHolder = UtilData Untyped (1, 1, 1) "source"

testEqValues = TestCase $ assertEqual
    "Should be true if the ConstValues are equal"
    True ((ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))) ==
    (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 1) "source"))))

testEqValuesFalse = TestCase $ assertEqual
    "Should be false if the ConstValues are not equal"
    False ((ConstValue (IntConstAST 2 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))) ==
    (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 1) "source"))))

testOrdValuesLT = TestCase $ assertEqual
    "Should return LT if first number is lower" "LT" 
    (show(compare(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))
    (ConstValue (IntConstAST 2 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))))

testOrdValuesGT = TestCase $ assertEqual
    "Should return GT if first number is lower" "GT" 
    (show(compare(ConstValue (IntConstAST 2 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))
    (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))))

testOrdValuesEQ = TestCase $ assertEqual
    "Should return EQ if first number is lower" "EQ" 
    (show(compare(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))
    (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))))

--testMatchTuple = TestCase $ assertBool
--    "Should be true"
--    (Right (Bindings (VarId, Values))) ==
--    (match (TupleValue [((ConstValue (IntConstAST 1 utilDataHolder), (TerValue (TypeId "typeName"))]))
--    (TuplePatternAST [(ConstPatternAST (IntConstAST 1 utilDataHolder) utilDataHolder), (TypePatternAST())]))

testTest = TestCase $ assertBool
    "sdf"
    ((Right (Bindings [])) ==
    (match (TupleValue [ConstValue (IntConstAST 42 utilDataHolder)])
    (TuplePatternAST [(ConstPatternAST (IntConstAST 42 utilDataHolder) utilDataHolder )] utilDataHolder)
    Set.empty))





main = runTestTT $ TestList [ testEqValues
                            , testEqValuesFalse
                            , testOrdValuesLT
                            , testOrdValuesGT
                            , testOrdValuesEQ
                            , testTest
                            ]