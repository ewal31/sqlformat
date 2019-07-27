{-# LANGUAGE OverloadedStrings #-}

module Parser.UtilSpec
  ( tests
  ) where

import qualified Data.Attoparsec.ByteString as BP
       (endOfInput, parseOnly, takeByteString, word8)
import Data.ByteString (ByteString)
import qualified Parser.Util as P
import Test.HUnit

tests = TestList [testByteStringToUpper, testParseAnyCase, testParseSubExp]

testByteStringToUpper =
  "upperB" ~:
  ["upperB select" ~: "SELECT" ~=? P.upperB "select", "upperB how!" ~: "HOW!" ~=? P.upperB "how!"]

testParseAnyCase =
  "parseAnyCase" ~:
  [ "SELECT in SELECTthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "SELECTthe rest"
  , "SELECT in selectthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "selectthe rest"
  , "SELECT in sELEctthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "sELEctthe rest"
  , "SELECT in sELEctthe rest" ~: Left "70: Failed reading: satisfy" ~=?
    BP.parseOnly (P.anyCaseString "friend" *> BP.takeByteString) "sELEctthe rest"
  ]

testParseSubExp =
  "parseSubExp" ~:
  [ "( SELECT B )" ~: Right "SELECT B" ~=?
    BP.parseOnly (P.parseSubExp' P.anyUntilThat) "( SELECT B )"
  , "( SELECT B )" ~: Right (Just "SELECT B") ~=?
    BP.parseOnly (P.parseSubExp P.anyUntilThat) "( SELECT B )"
  , "SELECT B" ~: Right Nothing ~=? BP.parseOnly (P.parseSubExp P.anyUntilThat) "SELECT B"
  ]
