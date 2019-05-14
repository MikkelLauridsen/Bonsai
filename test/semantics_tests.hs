import Semantics
import Ast
import Test.HUnit
import Data.Set as Set
import Data.Map as Map
import Data.Either

testCompareListsEQ = TestCase $ assertEqual "Should return EQ" EQ (compareLists [] [])
testCompareListsLT = TestCase $ assertEqual "Should return LT" LT (compareLists [] [(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))])
testCompareListsGT = TestCase $ assertEqual "Should return GT" GT (compareLists [(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))] [])
--testCompareListUnsupported = TestCase $ assertEqual "Unsupported list value" (error "unsupported list value type for comparison.") (compareLists [(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))] [(ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))])

testEvalCase = TestCase $ do result <- (evalCase [] (Map.empty) (Map.empty) (Set.fromList [(TypeId {typeName = "typeName"})]) (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
                             assertEqual "Should throw error message" "1:2: error: non-exhaustive case branches in:\n     source\n     ^^^^^^" (fromLeft "" result)


main = runTestTT $ TestList [testCompareListsEQ,
                             testCompareListsLT,
                             testCompareListsGT,
                             testEvalCase
                            ]

