import Control.Monad (void)
import qualified Parser.EquationSpec as Equation
import qualified Parser.SQLSpec as SQL
import qualified Parser.UtilSpec as Util
import Test.HUnit
import qualified TupleSpec

tests = [Equation.tests, Util.tests, SQL.tests, TupleSpec.tests]

main :: IO ()
main = mapM_ runTestTT tests
