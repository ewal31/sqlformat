{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Printer.SQLSpec where

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

instance P.Output String StringBuffer where
  write x = Buffer ((), x)

runWithBuffer' :: P.WriteAction StringBuffer -> P.Indent -> String
runWithBuffer' actions indent = m
  where
    (Buffer (a, m)) = P.runWriteAction actions indent

runWithBuffer :: P.Writer StringBuffer -> P.Indent -> String
runWithBuffer (P.Writer (actions, _)) indent = m
  where
    (Buffer (a, m)) = P.runWriteAction actions indent

tests = TestList [testWriteAction, testWriter]

testWriteAction =
  "writeAction" ~:
  [ "0" ~: "" ~=? runWithBuffer' mempty 0
  , "4" ~: "" ~=? runWithBuffer' mempty 4
  , "0" ~: "const" ~=? runWithBuffer' P.writeConst' 0
  , "0" ~: "constconst" ~=? runWithBuffer' (P.writeConst' <> P.writeConst') 0
  , "0" ~: "const\nconst" ~=? runWithBuffer' (P.writeConst' <> P.newline' <> P.writeConst') 0
  , "4" ~: "    const\n    const" ~=?
    runWithBuffer' (P.writeConst' <> P.newline' <> P.writeConst') 4
  ]

testWriter =
  "writer" ~:
  [ "0" ~: "" ~=? runWithBuffer mempty 0
  , "0" ~: "const" ~=? runWithBuffer P.writeConst 0
  , "0" ~: "const    const" ~=? runWithBuffer (P.writeConst <> P.indent <> P.writeConst) 0
  , "0" ~: "const    const    const" ~=?
    runWithBuffer (P.writeConst <> P.indent <> P.writeConst <> P.writeConst) 0
  , "0" ~: "const    constconst" ~=?
    runWithBuffer (P.writeConst <> P.indent <> P.writeConst <> P.undent <> P.writeConst) 0
  , "0" ~: "const        const" ~=?
    runWithBuffer (P.writeConst <> P.indent <> P.indent <> P.writeConst) 0
  ]
