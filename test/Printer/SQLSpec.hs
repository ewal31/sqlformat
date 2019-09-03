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
  write s c = Buffer (c <> ((\p -> P.C p mempty mempty mempty) . pure . length $ s), s)

runWithBuffer :: P.Writer StringBuffer P.Context -> P.Context -> (String, Sum Int)
runWithBuffer (P.Writer actions _) indent = (m, a)
  where
    (Buffer (P.C a _ _ _, m)) = P.runWriteAction actions indent

tests = TestList [testWriteAndMeasure]

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
  , "        string" ~: ("        string", pure 14) ~=?
    runWithBuffer (P.set' 8 (P.catchup' <> P.string "string")) mempty
  , "ONstring" ~: ("ONstring", pure 8) ~=?
    runWithBuffer (P.string "ON" <> P.set' 2 (P.catchup' <> P.string "string")) mempty
  , "     string" ~: ("     string", pure 11) ~=?
    runWithBuffer (P.catchup <> P.string "string") (P.C mempty mempty mempty (stackPush stackNew 5))
  , "WHERE\n     string" ~: ("WHERE\n     string", pure 11) ~=?
    runWithBuffer (P.string "WHERE" <> P.set (P.newline <> P.catchup <> P.string "string")) mempty
  ]
-- runWithBuffer' :: P.WriteAction StringBuffer P.Context -> P.Context -> String
-- runWithBuffer' actions indent = m
--   where
--     (Buffer (a, m)) = P.runWriteAction actions indent
-- 
-- tests = TestList [testWriteAndMeasure]
-- 
-- writeConst :: P.Writer m
-- writeConst = P.string "const"
-- testWriteAction =
--   "writeAction" ~:
--   [ "0" ~: "" ~=? runWithBuffer' mempty (P.Context 0 0)
--   , "4" ~: "" ~=? runWithBuffer' mempty (P.Context 4 0)
--   , "0" ~: "const" ~=? runWithBuffer' P.writeConst' (P.Context 0 0)
--   , "0" ~: "constconst" ~=? runWithBuffer' (P.writeConst' <> P.writeConst') (P.Context 0 0)
--   , "0" ~: "const\nconst" ~=?
--     runWithBuffer' (P.writeConst' <> P.newline' <> P.writeConst') (P.Context 0 0)
--   , "4" ~: "    const\n    const" ~=?
--     runWithBuffer' (P.writeConst' <> P.newline' <> P.writeConst') (P.Context 4 0)
--   ]
-- testWriter =
--   "writer" ~:
--   [ "0" ~: "" ~=? runWithBuffer mempty mempty
--   , "0" ~: "const" ~=? runWithBuffer P.writeConst mempty
--   , "0" ~: "const    const" ~=? runWithBuffer (P.writeConst <> P.indent <> P.writeConst) mempty
--   , "0" ~: "const    const    const" ~=?
--     runWithBuffer (P.writeConst <> P.indent <> P.writeConst <> P.writeConst) mempty
--   , "0" ~: "const    constconst" ~=?
--     runWithBuffer
--       (P.writeConst <> P.indent <> P.writeConst <> P.undent <> P.writeConst)
--       (P.Context 0 0)
--   , "0" ~: "const        const" ~=?
--     runWithBuffer (P.writeConst <> P.indent <> P.indent <> P.writeConst) mempty
--   ]
-- testWriteEquation =
--   "writeEquation" ~:
--   [ "VAL" ~: "x" ~=? runWithBuffer (P.writeEquation (AE.VAL "x")) mempty
--   , "EQU" ~: "x = 1" ~=? runWithBuffer (P.writeEquation (AE.EQU (AE.VAL "x") (AE.VAL "1"))) mempty
--   , "1 AND" ~: "x = 1\nAND y = 2" ~=?
--     runWithBuffer
--       (P.writeEquation
--          (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2"))))
--       mempty
--   , "2 AND" ~: "x = 1\nAND y = 2\nAND z = 3" ~=?
--     runWithBuffer
--       (P.writeEquation
--          (AE.AND
--             (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
--             (AE.EQU (AE.VAL "z") (AE.VAL "3"))))
--       mempty
--   -- , "AND" ~: "x = 1\n    AND y = 2\n    AND z = 3" ~=?
--   --   runWithBuffer
--   --     (P.writeEquation
--   --        (AE.AND
--   --           (AE.EQU (AE.VAL "z") (AE.VAL "3"))
--   --           (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))))
--   --     mempty
--   , "3 AND" ~: "x = 1\nAND y = 2\nAND z = 3\nAND a = 4" ~=?
--     runWithBuffer
--       (P.writeEquation
--          (AE.AND
--             (AE.AND
--                (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
--                (AE.EQU (AE.VAL "z") (AE.VAL "3")))
--             (AE.EQU (AE.VAL "a") (AE.VAL "4"))))
--       mempty
--   -- , "2 AND as WHERE" ~: "WHERE x = 1\nAND   y = 2\nAND   z = 3" ~=? "WHERE " ++
--   --   runWithBuffer
--   --     (P.writeEquation
--   --        (AE.AND
--   --           (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
--   --           (AE.EQU (AE.VAL "z") (AE.VAL "3"))))
--   --     (P.Context 1 2 0 0) -- as if with where justified
--   -- , "2 AND as ON" ~: "ON  x = 1\nAND y = 2\nAND z = 3" ~=? "ON " ++
--   --   runWithBuffer
--   --     (P.writeEquation
--   --        (AE.AND
--   --           (AE.AND (AE.EQU (AE.VAL "x") (AE.VAL "1")) (AE.EQU (AE.VAL "y") (AE.VAL "2")))
--   --           (AE.EQU (AE.VAL "z") (AE.VAL "3"))))
--   --     (P.Context 1 4 0 0) -- as if with justified and indented ON
--   ]
