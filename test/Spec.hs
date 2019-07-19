import Control.Monad (void)
import qualified ParserSpec
import qualified SQLParserSpec
import Test.HUnit
import qualified TupleSpec

tests = [ParserSpec.tests, SQLParserSpec.tests, TupleSpec.tests]

main :: IO ()
main = mapM_ runTestTT tests
