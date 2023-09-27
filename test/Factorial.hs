import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

import LargestProductFoldAndMap(doMain)
import LargestProductRec(doMain)
import LargestProductTailRec(doMain)

import FactorialDigitSumFoldAndMap(doMain)
import FactorialDigitSumInfitine(doMain)
import FactorialDigitSumRec(doMain)
import FactorialDigitSumTailRec(doMain)

-----------------------------------------------------------------------------------------

main :: IO ()
main = runTestTTAndExit tests

-----------------------------------------------------------------------------------------

test1 :: Test
test1 = TestCase (assertEqual "Test assert FoldAndMap" LargestProductFoldAndMap.doMain 70600674)

test2 :: Test
test2 = TestCase (assertEqual "Test assert Rec" LargestProductRec.doMain 70600674)

test3 :: Test
test3 = TestCase (assertEqual "Test assert TailRec" LargestProductTailRec.doMain 70600674)

-----------------------------------------------------------------------------------------

test4 :: Test
test4 = TestCase (assertEqual "Test assert FoldAndMap" FactorialDigitSumFoldAndMap.doMain 648)

test5 :: Test
test5 = TestCase (assertEqual "Test assert Rec" FactorialDigitSumInfitine.doMain 648)

test6 :: Test
test6 = TestCase (assertEqual "Test assert TailRec" FactorialDigitSumRec.doMain 648)

test7 :: Test
test7 = TestCase (assertEqual "Test assert TailRec" FactorialDigitSumTailRec.doMain 648)

-----------------------------------------------------------------------------------------

tests :: Test
tests = TestList [TestLabel "test1" test1,
                TestLabel "test2" test2,
                TestLabel "test3" test3,
                TestLabel "test3" test4,
                TestLabel "test3" test5,
                TestLabel "test3" test6,
                TestLabel "test3" test7]
