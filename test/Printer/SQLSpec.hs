{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, RankNTypes,
  MonoLocalBinds, FlexibleContexts, FlexibleInstances #-}

module Printer.SQLSpec where

import qualified AST.Equation as AE
import Data.Monoid (Sum(..))
import Data.Stack
import qualified Printer.SQL as P
import Test.HUnit

newtype Buffer m a = Buffer
  { getBuffer :: (a, m)
  } deriving (Eq, Show)

instance (Monoid f) => Functor (Buffer f) where
  fmap f (Buffer (x, m)) = Buffer (f x, m)

instance (Monoid f) => Applicative (Buffer f) where
  pure x = Buffer (x, mempty)
  (Buffer (f, m)) <*> x = fmap f x

instance (Monoid m) => Monad (Buffer m) where
  return x = Buffer (x, mempty)
  (Buffer (x, m)) >>= f = Buffer (y, m <> n)
    where
      (Buffer (y, n)) = f x
  (Buffer (x, m)) >> (Buffer (y, n)) = Buffer (y, m <> n)

type StringBuffer = Buffer String

instance P.Output String StringBuffer P.Context where
  write s c = Buffer (c <> (P.fromPos . pure . length $ s), s)

runWithBuffer :: P.Writer StringBuffer P.Context -> P.Context -> (String, Sum Int)
runWithBuffer actions context = (m, P.pos c)
  where
    (Buffer (c, m)) = P.runWriter actions context

tests = TestList [testWriteAndMeasure, testWriteEquation]

testWriteAndMeasure =
  "testWriteAndMeasure" ~:
  [ "" ~: ("", pure 0) ~=? runWithBuffer mempty mempty
  , "string" ~: ("string", pure 6) ~=? runWithBuffer (P.string "string") mempty
  , "string1 string2" ~: ("string1 string2", pure 15) ~=?
    runWithBuffer (P.string "string1" <> P.string " " <> P.string "string2") mempty
  , "\n    string" ~: ("\n    string", pure 10) ~=?
    runWithBuffer (P.indent $ P.newline <> P.string "string") mempty
  , "string1\n    string2" ~: ("string1\n    string2", pure 11) ~=?
    runWithBuffer (P.string "string1" <> P.indent (P.newline <> P.string "string2")) mempty
  , "Manual Set Catchup" ~: ("          string", pure 16) ~=?
    runWithBuffer (P.catchup P.set1 <> P.string "string") (P.fromSet1 . pure $ 10)
  , "WHERE like Set and Catchup" ~: ("WHERE\n     string", pure 11) ~=?
    runWithBuffer
      (P.string "WHERE" <>
       P.set P.set1 P.fromSet1 (P.newline <> P.catchup P.set1 <> P.string "string"))
      mempty
  , "Embedded Set and Catchup" ~: ("WHERE\n     string", pure 11) ~=?
    runWithBuffer
      (P.string "WHERE" <>
       P.set P.set1 P.fromSet1 (P.newline <> P.catchup P.set1 <> P.string "string"))
      (P.fromSet1 . pure $ 10)
  , "Embedded Set and Catchup 2" ~: ("0123456789\nWHERE\n     string", pure 11) ~=?
    runWithBuffer
      (P.string "0123456789" <>
       P.set
         P.set1
         P.fromSet1
         (P.newline <> P.string "WHERE" <>
          P.set P.set1 P.fromSet1 (P.newline <> P.catchup P.set1 <> P.string "string")))
      mempty
  ]

testWriteEquation =
  "writeEquation" ~:
  [ "VAL" ~: ("x", pure 1) ~=? runWithBuffer (P.writeEquation (AE.VAL "x")) mempty
  , "EQU" ~: ("x = 1", pure 5) ~=?
    runWithBuffer (P.writeEquation (AE.EQU (AE.VAL "x") (AE.VAL "1"))) mempty
  , "1 AND" ~: ("x = 1\nAND y = 2", pure 9) ~=?
    runWithBuffer
      (P.writeEquation
         (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2"))))
      mempty
  , "2 AND" ~: ("x = 1\nAND y = 2\nAND z = 3", pure 9) ~=?
    runWithBuffer
      (P.writeEquation
         (AE.AND
            (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
            (AE.EQU (AE.VAL "z") (AE.VAL "3"))))
      mempty
  , "3 AND" ~: ("x = 1\nAND y = 2\nAND z = 3\nAND a = 4", pure 9) ~=?
    runWithBuffer
      (P.writeEquation
         (AE.AND
            (AE.AND
               (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
               (AE.EQU (AE.VAL "z") (AE.VAL "3")))
            (AE.EQU (AE.VAL "a") (AE.VAL "4"))))
      mempty
  , "2 AND as WHERE" ~: ("WHERE x = 1\nAND   y = 2\nAND   z = 3", pure 11) ~=?
    runWithBuffer
      (P.string "WHERE" P.<->
       P.set
         P.set1
         P.fromSet1
         (P.writeEquation
            (AE.AND
               (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
               (AE.EQU (AE.VAL "z") (AE.VAL "3")))))
      (P.fromSet1 . pure $ 5) -- as if with where justified
  , "2 AND as ON" ~: ("ON  x = 1\nAND y = 2\nAND z = 3", pure 9) ~=?
    runWithBuffer
      (P.string "ON  " <>
       P.set
         P.set1
         P.fromSet1
         (P.writeEquation
            (AE.AND
               (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
               (AE.EQU (AE.VAL "z") (AE.VAL "3")))))
      (P.fromSet1 . pure $ 5) -- as if with on justified
  ]
