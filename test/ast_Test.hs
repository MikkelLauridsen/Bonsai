import Ast
import Test.HUnit

-- UtilData placeholder
utilDataHolder = UtilData (1, 1, 1) "source"

-- Compares two values
-- Returns LT, since value one is lower than value 2
testOrdTypes = TestCase $ assertEqual
    "Should return LT if first float is less than last float" "LT" (show(compare 1.01 1.02))

-- Test whether 2 and 3 are equal or not
-- Returns False, since they aren't equal
testOrdTypesFail = TestCase $ assertEqual
    "Should return False if compare set to EQ but it is not" False (show(compare 2 3) == "EQ")

-- Compares the TypeIds
-- Returns False, since the TypeIds are not equal
testEqTypeId = TestCase $ assertEqual
    "Should be False if not equal, also with difference with small and big chars" False ((TypeId "TypeName") == (TypeId "typeName"))

-- Compares the length of the two TypeIds
-- Returns GT, since the first TypeId is larger than the second TypeId
testOrdTypeId = TestCase $ assertEqual
    "Should return GT if first typeId is longer" "GT" (show(compare (TypeId "TypeNames") (TypeId "TypeName")))

-- Compares two VarIds
-- Returns True, since the VarIds are equal
testEQVarId = TestCase $ assertEqual
    "Should be True as long the types are EQ" 
    True ((VarId "name") == (VarId "name"))

--Compares two VarIds
-- Returns False, since the VarIds are not equal
testEQVarIdFalse = TestCase $ assertEqual
    "Should be False if the VarId isn't the same" 
    False ((VarId "name1") == (VarId "name2"))

-- Compares two IntConstASTs
-- Returns True, since the IntConstAST are equal
testEQIntConstAST = TestCase $ assertEqual
    "Should be True, since the IntConstAST are equal" 
    True ((IntConstAST 1 utilDataHolder) == 
        (IntConstAST 1 utilDataHolder))

-- Compares two IntConstASTs
-- Returns False, since the IntConstASTs are not equal
testEQIntConstASTFalse = TestCase $ assertEqual
    "Should be False because the value are different, although the position is the same" 
    False ((IntConstAST 2 utilDataHolder) == 
        (IntConstAST 1 utilDataHolder))

-- main, which runs all the testcases
main = runTestTT $ TestList [ testOrdTypes
                            , testOrdTypesFail
                            , testEqTypeId
                            , testOrdTypeId
                            , testEQVarId
                            , testEQVarIdFalse
                            , testEQIntConstAST
                            , testEQIntConstASTFalse
                            ]