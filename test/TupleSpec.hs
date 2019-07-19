module TupleSpec
  ( tests
  ) where

import Test.HUnit
import qualified Tuple as T

tests = TestList [testApplyFnToTuple]

testApplyFnToTuple =
  "applyFnToTuple" ~:
  [ "applyFnToTuple (\"hello\", ('y', ('s', ()))) $ \a b c -> a ++ [b, c]" ~: "helloys" ~=?
    T.applyFnToTuple ("hello", ('y', ('s', ()))) (\a b c -> a ++ [b, c])
  ]
