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

tests = TestList [testParseFunction, testParseCase, testParseEquation]

testParseFunction =
  "parseFunction" ~:
  [ "MAX(id)" ~: Right (A.FUNC "MAX" [A.VAL "id"], ()) ~=?
    BP.parseOnly (P.parseFunction "MAX" BP.endOfInput) "id)"
  , "IF(id, 0, 1)" ~: Right (A.FUNC "IF" [A.VAL "id", A.VAL "0", A.VAL "1"], ()) ~=?
    BP.parseOnly (P.parseFunction "IF" BP.endOfInput) "id, 0, 1)"
  , "IF(  id  , 0  , 1  )" ~: Right (A.FUNC "IF" [A.VAL "id", A.VAL "0", A.VAL "1"], ()) ~=?
    BP.parseOnly (P.parseFunction "IF" BP.endOfInput) "id, 0, 1)"
  -- , "ABS(-20)" ~: Right (A.FUNC "ABS" [A.VAL "-20"], ()) ~=?
  --   BP.parseOnly (P.parseFunction "ABS" BP.endOfInput) "-20)"
  , "CONCAT('Me', 'You', 'Someone')" ~:
    Right (A.FUNC "CONCAT" [A.VAL "'Me'", A.VAL "'You'", A.VAL "'Someone'"], ()) ~=?
    BP.parseOnly (P.parseFunction "CONCAT" BP.endOfInput) "'Me', 'You', 'Someone')"
  , "MAX(a * b)" ~: Right (A.FUNC "MAX" [A.TIMES (A.VAL "a") (A.VAL "b")], ()) ~=?
    BP.parseOnly (P.parseFunction "MAX" BP.endOfInput) "a * b)"
  , "MAX(a * b, c + d)" ~:
    Right (A.FUNC "MAX" [A.TIMES (A.VAL "a") (A.VAL "b"), A.PLUS (A.VAL "c") (A.VAL "d")], ()) ~=?
    BP.parseOnly (P.parseFunction "MAX" BP.endOfInput) "a * b, c + d)"
  ]

testParseCase =
  "parseCase" ~:
  [ "CASE x WHEN 1 THEN 'one' WHEN 2 THEN 'two' END" ~:
    Right
      ( A.CASE
          (Just (A.VAL "x"))
          [A.WHENTHEN (A.VAL "1") (A.VAL "'one'"), A.WHENTHEN (A.VAL "2") (A.VAL "'two'")]
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseCase BP.endOfInput) "x WHEN 1 THEN 'one' WHEN 2 THEN 'two' END"
  , "CASE x WHEN 1 = a THEN 'one' END" ~:
    Right
      ( A.CASE
          (Just (A.VAL "x"))
          [A.WHENTHEN (A.EQU (A.VAL "1") (A.VAL "a")) (A.VAL "'one'")]
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseCase BP.endOfInput) "x WHEN 1 = a THEN 'one' END"
  , "CASE x WHEN 1 = a THEN 'one' WHEN 2 = b THEN 'two' END" ~:
    Right
      ( A.CASE
          (Just (A.VAL "x"))
          [ A.WHENTHEN (A.EQU (A.VAL "1") (A.VAL "a")) (A.VAL "'one'")
          , A.WHENTHEN (A.EQU (A.VAL "2") (A.VAL "b")) (A.VAL "'two'")
          ]
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseCase BP.endOfInput) "x WHEN 1 = a THEN 'one' WHEN 2 = b THEN 'two' END"
  , "CASE WHEN x = 1 THEN 'one' WHEN x = 2 THEN 'two' END" ~:
    Right
      ( A.CASE
          Nothing
          [ A.WHENTHEN (A.EQU (A.VAL "x") (A.VAL "1")) (A.VAL "'one'")
          , A.WHENTHEN (A.EQU (A.VAL "x") (A.VAL "2")) (A.VAL "'two'")
          ]
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseCase BP.endOfInput) "WHEN x = 1 THEN 'one' WHEN x = 2 THEN 'two' END"
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
  , "a = concat(a, b)" ~: Right (A.EQU (A.VAL "a") (A.FUNC "concat" [A.VAL "a", A.VAL "b"]), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a = concat(a, b)"
  , "MAX(a * b)" ~: Right (A.FUNC "MAX" [A.TIMES (A.VAL "a") (A.VAL "b")], ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "MAX(a * b)"
  , "a = MAX(a * b)" ~:
    Right (A.EQU (A.VAL "a") (A.FUNC "MAX" [A.TIMES (A.VAL "a") (A.VAL "b")]), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "a = MAX(a * b)"
  , "MAX(id)" ~: Right (A.FUNC "MAX" [A.VAL "id"], ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "MAX(id)"
  , "CASE x WHEN 1 THEN 'one' WHEN 2 THEN 'two' END" ~:
    Right
      ( A.CASE
          (Just (A.VAL "x"))
          [A.WHENTHEN (A.VAL "1") (A.VAL "'one'"), A.WHENTHEN (A.VAL "2") (A.VAL "'two'")]
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "CASE x WHEN 1 THEN 'one' WHEN 2 THEN 'two' END"
  , "CASE WHEN x = 1 THEN 'one' WHEN x = 2 THEN 'two' END" ~:
    Right
      ( A.CASE
          Nothing
          [ A.WHENTHEN (A.EQU (A.VAL "x") (A.VAL "1")) (A.VAL "'one'")
          , A.WHENTHEN (A.EQU (A.VAL "x") (A.VAL "2")) (A.VAL "'two'")
          ]
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseEquation BP.endOfInput)
      "CASE WHEN x = 1 THEN 'one' WHEN x = 2 THEN 'two' END"
  , "(a + 2) * 1 = 1" ~:
    Right (A.EQU (A.TIMES (A.PLUS (A.VAL "a") (A.VAL "2")) (A.VAL " 1")) (A.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "(a + 2) * 1 = 1"
  , "SUM((a + 2) * 1) = 1" ~:
    Right
      ( A.EQU (A.FUNC "SUM" [A.TIMES (A.PLUS (A.VAL "a") (A.VAL "2")) (A.VAL " 1")]) (A.VAL " 1")
      , ()) ~=?
    BP.parseOnly (P.parseEquation BP.endOfInput) "SUM((a + 2) * 1) = 1"
  ]
