import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

main :: IO ()
main = runTestTTAndExit tests

test1 :: Test
test1 = TestCase (assertEqual "Test assert" True True)

tests :: Test
tests = TestList [TestLabel "test1" test1]
