import Control.Monad (void)
import qualified ParserSpec
import Test.HUnit
import qualified TupleSpec

tests = [ParserSpec.tests, TupleSpec.tests]

main :: IO ()
main = mapM_ runTestTT tests
