import Control.Monad (void)
import qualified Parser.EquationSpec as Equation
import qualified Parser.SQLSpec as SQL
import qualified Parser.UtilSpec as Util
import qualified Printer.SQLSpec as SQLPrint
import Test.HUnit
import qualified TupleSpec

tests = [Equation.tests, Util.tests, SQL.tests, SQLPrint.tests, TupleSpec.tests]

main :: IO ()
main = mapM_ runTestTT tests
