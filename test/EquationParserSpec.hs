{-# LANGUAGE OverloadedStrings #-}

module EquationParserSpec
  ( tests
  ) where

import qualified AST as A (ELSE, EQUATION(..), WHENTHEN(..))
import qualified Data.Attoparsec.ByteString as BP
       (endOfInput, parseOnly, takeByteString, word8)
import Data.ByteString (ByteString)
import qualified EquationParser as P
import Test.HUnit

tests = TestList [testParseFunction]

testParseFunction =
  "parseFunction" ~:
  [ "MAX(id)" ~: Right (A.FUNC "MAX" [A.VAL "id"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "MAX(id)"
  , "IF(id, 0, 1)" ~: Right (A.FUNC "IF" [A.VAL "id", A.VAL "0", A.VAL "1"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "IF(id, 0, 1)"
  , "ABS(-20)" ~: Right (A.FUNC "ABS" [A.VAL "-20"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "ABS(-20)"
  , "CONCAT('Me', 'You', 'Someone')" ~:
    Right (A.FUNC "CONCAT" [A.VAL "'Me'", A.VAL "'You'", A.VAL "'Someone'"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "CONCAT('Me', 'You', 'Someone')"
  ]
