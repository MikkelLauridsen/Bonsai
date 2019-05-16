import Semantics
import Test.HUnit
import Ast
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Either


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

testMatchList = TestCase $ assertBool
    "Should be true if the right bindings are returned"
    ((Right (Bindings [((VarId "varName" (Typed (PrimType IntPrim))), ConstValue (IntConstAST 1 utilDataHolder))])) ==
    (match (ListValue [ ConstValue (IntConstAST 1 utilDataHolder)])
    (ListPatternAST [VarPatternAST (VarId "varName" (Typed (PrimType IntPrim))) utilDataHolder] utilDataHolder)
    Set.empty))

testMatchTuple = TestCase $ assertBool
    "Should return Right (Bindings []) because no new values added to the binding list"
    ((Right (Bindings [])) ==
    (match (TupleValue [ConstValue (IntConstAST 42 utilDataHolder)])
    (TuplePatternAST [(ConstPatternAST (IntConstAST 42 utilDataHolder) utilDataHolder )] utilDataHolder)
    Set.empty))

testMatchTerCons = TestCase $ assertBool
    "Should return an binding of the varid and the value"
    ((Right (Bindings [((VarId "VarName" Untyped), (TerValue (TypeId "TypeId")))])) ==
    (match (TerConsValue (TypeId "TestCons") (TerValue (TypeId "TypeId")))
    (TypeConsPatternAST (TypeId "TestCons") (VarPatternAST (VarId "VarName" Untyped) utilDataHolder) utilDataHolder)
    (Set.singleton (TypeId "TestCons"))))
  
testEvalLetIn = TestCase $ do 
      result <- (evalLetIn (UntypedVarAST (VarId "X" (Typed (PrimType IntPrim))) utilDataHolder) 
                (VarExprAST (VarId "VarName" (Typed (PrimType IntPrim))) utilDataHolder) 
                (VarExprAST (VarId "X" (Typed (PrimType IntPrim))) utilDataHolder) 
                (Map.singleton (VarId "VarName" (Typed (PrimType IntPrim))) (ConstValue (IntConstAST 42 utilDataHolder))) 
                 Map.empty
                 Set.empty)
      assertBool
          "test"
          (result == (Right ((ConstValue (IntConstAST 42 utilDataHolder)), Map.empty)))
    


testEvalCase = TestCase $ do 
            result <- (evalCase [] 
              (Map.fromList []) 
              (Map.fromList []) 
              (Set.fromList [(TypeId {typeName = "ok"})]) 
              (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
            assertEqual "test" "1:2: error: non-exhaustive case branches in:\n     source\n     ^^^^^^" (fromLeft "" result)
        
        
testEvalCaseNoErrorMessage = TestCase $ do 
                          result <- (evalCase [((PredWildAST (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")), ((TypeExprAST (TypeId {typeName = "ok"}) (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))))] 
                            (Map.fromList []) 
                            (Map.fromList []) 
                            (Set.fromList [(TypeId {typeName = "ok"})]) 
                            (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
                          assertEqual "Shouldn't return error message" "" (fromLeft "" result)
        
testEvalCaseNoErrorData = TestCase $ do 
                       result <- (evalCase [((PredWildAST (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")), ((TypeExprAST (TypeId {typeName = "ok"}) (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))))] 
                         (Map.fromList []) 
                         (Map.fromList []) 
                         (Set.fromList [(TypeId {typeName = "ok"})]) 
                         (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
                       assertEqual "Should return 0" 0 (Map.size (snd (fromRight ((TupleValue []), (Map.fromList [])) result)))
        
testEvalMatch = TestCase $ do
             result <- (evalMatch (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))
               []
               (Map.empty)
               (Map.empty)
               (Set.fromList[(TypeId {typeName = "typeName"})])
               (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
             assertEqual "Should return error message" "1:2: error: non-exhaustive match branches in:\n     source\n     ^^^^^^" (fromLeft "" result)
        
testEvalMatchNoErrorMessage = TestCase $ do
                           result <- (evalMatch (ConstValue (IntConstAST 1 (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")))
                             [((WildPatternAST (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source")), ((TypeExprAST (TypeId {typeName = "typeName"}) (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))))]
                             (Map.empty)
                             (Map.empty)
                             (Set.fromList[(TypeId {typeName = "typeName"})])
                             (UtilData (Typed (PrimType IntPrim)) (1, 2, 3) "source"))
                           assertEqual "Should return error message" "" (fromLeft "" result)


main = runTestTT $ TestList [ testEqValues
                            , testEqValuesFalse
                            , testOrdValuesLT
                            , testOrdValuesGT
                            , testOrdValuesEQ
                            , testMatchTuple
                            , testMatchList
                            , testMatchTerCons
                            , testEvalLetIn
                            , testEvalCase
                            , testEvalCaseNoErrorMessage
                            , testEvalCaseNoErrorData
                            , testEvalMatch
                            , testEvalMatchNoErrorMessage
                            ]
