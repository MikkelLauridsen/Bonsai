-- Unit testing the Semantics file
-- Testing functions such as Match, Case and Let in

-- Imports the Semantics and AST files
-- Imports Map, Set and Either so they can be used in the functions
import Semantics
import Test.HUnit
import Ast
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Either

-- UtilData placeholder
utilDataHolder = UtilData (1, 1, 1) "source"

-- Checks if two values are equal
-- Returns True, since both values are equal
testEqValues = TestCase $ assertEqual
    "Should be True if the ConstValues are equal"
    True ((ConstValue (IntConstAST 1 utilDataHolder)) ==
    (ConstValue (IntConstAST 1 utilDataHolder)))

-- Returns False, since both values are not equal
testEqValuesFalse = TestCase $ assertEqual
    "Should be False if the ConstValues are not equal"
    False ((ConstValue (IntConstAST 2 utilDataHolder)) ==
    (ConstValue (IntConstAST 1 utilDataHolder)))

-- Checks whether the first value is lower or greater than value two
-- Returns "LT", since value one is lower than value two
testOrdValuesLT = TestCase $ assertEqual
    "Should return LT if first number is lower" "LT" 
    (show(compare(ConstValue (IntConstAST 1 utilDataHolder))
    (ConstValue (IntConstAST 2 utilDataHolder))))

-- Returns "GT", since value one is greather than value two
testOrdValuesGT = TestCase $ assertEqual
    "Should return GT if first number is higher" "GT" 
    (show(compare(ConstValue (IntConstAST 2 utilDataHolder))
    (ConstValue (IntConstAST 1 utilDataHolder))))

-- Returns "EQ", since value one and value two are equal
testOrdValuesEQ = TestCase $ assertEqual
    "Should return EQ if first number is lower" "EQ" 
    (show(compare(ConstValue (IntConstAST 1 utilDataHolder))
    (ConstValue (IntConstAST 1 utilDataHolder))))

-- Testcase for the match function with a list
-- Returns right bindings
testMatchList = TestCase $ assertBool
    "Should be True if the right bindings are returned"
    ((Right (Bindings [((VarId "varName"), ConstValue (IntConstAST 1 utilDataHolder))])) ==
    (match (ListValue [ ConstValue (IntConstAST 1 utilDataHolder)])
    (ListPatternAST [VarPatternAST (VarId "varName") utilDataHolder] utilDataHolder)
    Set.empty))

-- Match function with a tuple
-- Returns Right (Bindings [])
testMatchTuple = TestCase $ assertBool
    "Should return Right (Bindings []) because no new values added to the binding list"
    ((Right (Bindings [])) ==
    (match (TupleValue [ConstValue (IntConstAST 42 utilDataHolder)])
    (TuplePatternAST [(ConstPatternAST (IntConstAST 42 utilDataHolder) utilDataHolder )] utilDataHolder)
    Set.empty))

-- Match function with TerCons
-- Returns the bindings of the VarId and value
testMatchTerCons = TestCase $ assertBool
    "Should return a binding of the VarId and the value"
    ((Right (Bindings [((VarId "VarName"), (TerValue (TypeId "TypeId")))])) ==
    (match (TerConsValue (TypeId "TestCons") (TerValue (TypeId "TypeId")))
    (TypeConsPatternAST (TypeId "TestCons") (VarPatternAST (VarId "VarName") utilDataHolder) utilDataHolder)
    (Set.singleton (TypeId "TestCons"))))

-- Testcase for evalLetIn
testEvalLetIn = TestCase $ do 
      result <- (evalLetIn (UntypedVarAST (VarId "X") utilDataHolder) 
                (VarExprAST (VarId "VarName") utilDataHolder) 
                (VarExprAST (VarId "X") utilDataHolder) 
                (Map.singleton (VarId "VarName") (ConstValue (IntConstAST 42 utilDataHolder))) 
                 Map.empty
                 Set.empty)
      assertBool
          "test"
          (result == (Right ((ConstValue (IntConstAST 42 utilDataHolder)), Map.empty)))
    

-- Checks whether will return an error or not
-- In this case it returns an error
testEvalCase = TestCase $ do 
            result <- (evalCase [] 
              (Map.fromList []) 
              (Map.fromList []) 
              (Set.fromList [(TypeId {typeName = "ok"})]) 
              utilDataHolder)
            assertEqual "Should return error message" "1:1: error: non-exhaustive case branches in:\n   source\n   ^^^^^^" (fromLeft "" result)
        
-- Doesn't return error message        
testEvalCaseNoErrorMessage = TestCase $ do 
                          result <- (evalCase [((PredWildAST utilDataHolder), ((TypeExprAST (TypeId {typeName = "ok"}) utilDataHolder)))] 
                            (Map.fromList []) 
                            (Map.fromList []) 
                            (Set.fromList [(TypeId {typeName = "ok"})]) 
                            utilDataHolder)
                          assertEqual "Shouldn't return error message" "" (fromLeft "" result)

-- Another test for Case, where it doesn't return any erros        
testEvalCaseNoErrorData = TestCase $ do 
                       result <- (evalCase [((PredWildAST utilDataHolder), ((TypeExprAST (TypeId {typeName = "ok"}) utilDataHolder)))] 
                         (Map.fromList []) 
                         (Map.fromList []) 
                         (Set.fromList [(TypeId {typeName = "ok"})]) 
                         utilDataHolder)
                       assertEqual "Should return 0" 0 (Map.size (snd (fromRight ((TupleValue []), (Map.fromList [])) result)))

-- Testcase for the match function
-- Returns error message
testEvalMatch = TestCase $ do
             result <- (evalMatch (ConstValue (IntConstAST 1 utilDataHolder))
               []
               (Map.empty)
               (Map.empty)
               (Set.fromList[(TypeId {typeName = "typeName"})])
               utilDataHolder)
             assertEqual "Should return error message" "1:1: error: non-exhaustive match branches in:\n   source\n   ^^^^^^" (fromLeft "" result)

-- Doesn't return error message
testEvalMatchNoErrorMessage = TestCase $ do
                           result <- (evalMatch (ConstValue (IntConstAST 1 utilDataHolder))
                             [((WildPatternAST utilDataHolder), ((TypeExprAST (TypeId {typeName = "typeName"}) utilDataHolder)))]
                             (Map.empty)
                             (Map.empty)
                             (Set.fromList[(TypeId {typeName = "typeName"})])
                             utilDataHolder)
                           assertEqual "Should return error message" "" (fromLeft "" result)

-- Main which runs all the testcases
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
