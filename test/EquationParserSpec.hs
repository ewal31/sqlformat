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

tests = TestList [testParseBoolSymbol, testParseFunction, testParseEquation]

testParseBoolSymbol =
  "parseBoolSymbol" ~:
  [ "a =" ~: Right (A.VAL "a") ~=? fst <$> BP.parseOnly P.parseBoolSymbol "a ="
  , "a +" ~: Right (A.VAL "a") ~=? fst <$> BP.parseOnly P.parseBoolSymbol "a +"
  ]

testParseFunction =
  "parseFunction" ~:
  [ "MAX(id)" ~: Right (A.FUNC "MAX" [A.VAL "id"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "MAX(id)"
  , "IF(id, 0, 1)" ~: Right (A.FUNC "IF" [A.VAL "id", A.VAL "0", A.VAL "1"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "IF(id, 0, 1)"
  -- , "ABS(-20)" ~: Right (A.FUNC "ABS" [A.VAL "-20"], ()) ~=?
  --   BP.parseOnly (P.parseFunction BP.endOfInput) "ABS(-20)"
  , "CONCAT('Me', 'You', 'Someone')" ~:
    Right (A.FUNC "CONCAT" [A.VAL "'Me'", A.VAL "'You'", A.VAL "'Someone'"], ()) ~=?
    BP.parseOnly (P.parseFunction BP.endOfInput) "CONCAT('Me', 'You', 'Someone')"
  ]

testParseEquation =
  "parseEquation" ~:
  [ "a" ~: Right (A.VAL "a", ()) ~=? BP.parseOnly (P.parseEquation BP.endOfInput) "a"
  , "a = 1" ~: Right (A.EQU (A.VAL "a") (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a = 1"
  , "a = b + 2" ~: Right (A.EQU (A.VAL "a") (A.PLUS (A.VAL "b") (A.VAL "2")), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a = b + 2"
  , "a + 2 = 1" ~: Right (A.EQU (A.PLUS (A.VAL "a") (A.VAL "2")) (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a + 2 = 1"
  , "a + 2 = b + 1" ~:
    Right (A.EQU (A.PLUS (A.VAL "a") (A.VAL "2")) (A.PLUS (A.VAL "b") (A.VAL "1")), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a + 2 = b + 1"
  , "a * 2 = 1" ~: Right (A.EQU (A.TIMES (A.VAL "a") (A.VAL "2")) (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a * 2 = 1"
  , "a * 2 + 1 = 1" ~:
    Right (A.EQU (A.PLUS (A.TIMES (A.VAL "a") (A.VAL "2")) (A.VAL "1")) (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a * 2 + 1 = 1"
  , "a + 2 * 1 = 1" ~:
    Right (A.EQU (A.PLUS (A.VAL "a") (A.TIMES (A.VAL "2") (A.VAL "1"))) (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a + 2 * 1 = 1"
  , "1 = a * 2 + 1" ~:
    Right (A.EQU (A.VAL "1") (A.PLUS (A.TIMES (A.VAL "a") (A.VAL "2")) (A.VAL "1")), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "1 = a * 2 + 1"
  , "1 = a + 2 * 1" ~:
    Right (A.EQU (A.VAL "1") (A.PLUS (A.VAL "a") (A.TIMES (A.VAL "2") (A.VAL "1"))), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "1 = a + 2 * 1"
  , "1 / 23 = a + 2 * 1 - b" ~:
    Right
      ( A.EQU
          (A.DIV (A.VAL "1") (A.VAL "23"))
          (A.MINUS (A.PLUS (A.VAL "a") (A.TIMES (A.VAL "2") (A.VAL "1"))) (A.VAL "b"))
      , ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "1 / 23 = a + 2 * 1 - b"
  ]
