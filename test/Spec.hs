
import Test.Tasty
import Test.Tasty.HUnit

import Render.Base (expander)
import HtmlTest

main :: IO ()
main = -- defaultMain tests
  expanderTest >>= print

tests = testGroup "tests" [ unitTests ]

unitTests = testGroup "Unit tests"
  [
    testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1,2] @?= GT
  ]

expanderTest = do
  expected <- fmap canonicalizeTrees $ readHTML "/tmp/expander.html"
  let got = canonicalizeBlaze (expander "#windows")
  runDiff got expected

