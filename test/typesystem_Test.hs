import Typesystem
import Semantics
import Test.HUnit
import Ast
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Either

utilDataHolder = UtilData Untyped (1, 1, 1) "source"





main = runTestTT $ TestList [  ]