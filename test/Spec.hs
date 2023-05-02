import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Checked"
      [testCase "two plus two" $ 2 + 2 @?= (4 :: Int)]
