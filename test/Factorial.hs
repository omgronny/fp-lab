import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

import LargestProductFoldAndMap(doMain)
import LargestProductRec(doMain)
import LargestProductTailRec(doMain)

main :: IO ()
main = runTestTTAndExit tests

test1 :: Test
test1 = TestCase (assertEqual "Test assert FoldAndMap" LargestProductFoldAndMap.doMain 70600674)

test2 :: Test
test2 = TestCase (assertEqual "Test assert Rec" LargestProductRec.doMain 70600674)

test3 :: Test
test3 = TestCase (assertEqual "Test assert TailRec" LargestProductTailRec.doMain 70600674)

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]
