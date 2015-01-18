import Test.Tasty

import qualified NodeTest as N
import qualified OperationTest as O
import qualified TransformationTest as T

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [N.tests, O.tests, T.tests]
