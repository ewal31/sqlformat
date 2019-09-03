{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  FunctionalDependencies, RankNTypes #-}

module Printer.SQL where

import AST.Equation
import Control.Monad ((>=>))
import Data.ByteString.Char8 (unpack)
import Data.Monoid (Sum(..))
import Data.Stack

data Context =
  C (Sum Int)
    (Sum Int)
    (Sum Int)
    (Stack Int)

instance Semigroup Context where
  (C p1 i1 j1 s1) <> (C p2 i2 j2 s2) = C (p1 <> p2) (i1 <> i2) (j1 <> j2) (s1 <> s2)

instance Monoid Context where
  mempty = C mempty mempty mempty mempty

newtype WriteAction m a = WA
  { runWriteAction :: (Monad m) =>
                        a -> m a
  }

instance (Monad m, Monoid a) => Semigroup (WriteAction m a) where
  (WA w1) <> (WA w2) = WA $ w1 >=> w2

instance (Monad m, Monoid a) => Monoid (WriteAction m a) where
  mempty = WA pure

data Writer m a =
  Writer (WriteAction m a)
         a

instance (Monad m, Monoid a) => Semigroup (Writer m a) where
  (Writer m1 a1) <> (Writer m2 a2) = Writer (m1 <> (a1 · m2)) (a1 <> a2)
    where
      a · (WA m) = WA $ \c -> m (a <> c)

instance (Monad m, Monoid a) => Monoid (Writer m a) where
  mempty = Writer mempty mempty

class (Monad m) =>
      Output w m b | m -> w where
  write :: w -> b -> m b

instance Output String IO Context where
  write s c = do
    print s
    return $ c <> ((\p -> C p mempty mempty mempty) . pure . length $ s)

string :: (Output String m a, Monoid a) => String -> Writer m a
string s = Writer (WA $ write s) mempty

indent :: (Monad m) => Writer m Context -> Writer m Context
indent w =
  Writer mempty (C mempty (pure 4) mempty mempty) <> w <>
  Writer mempty (C mempty (pure (-4)) mempty mempty)

set :: (Monad m) => Writer m Context -> Writer m Context
set w =
  Writer (WA $ pure . build) mempty <> w <>
  Writer (WA $ pure . unbuild) (C mempty mempty mempty mempty)
  where
    build (C (Sum p) i j s) = C (Sum p) i j (stackPush s p)
    unbuild (C p i j s) = C p i j (unpack . stackPop $ s)
    unpack (Just (s, _)) = s

set' :: (Monad m) => Int -> Writer m Context -> Writer m Context
set' i w =
  Writer mempty (C mempty mempty (pure i) mempty) <> w <>
  Writer mempty (C mempty mempty (pure (-i)) mempty)

newline :: (Output String m Context, Monad m) => Writer m Context
newline =
  Writer
    (WA $ \c -> write ('\n' : (run . indent $ c)) (C (pure (-1)) mempty mempty (stack c)))
    mempty
  where
    run 0 = ""
    run ind = ' ' : run (ind - 1)
    indent (C _ i _ _) = i
    stack (C _ _ _ s) = s

catchup' :: (Output String m Context, Monad m) => Writer m Context
catchup' = Writer (WA $ \c -> write (run . diff $ c) c) mempty
  where
    run 0 = ""
    run ind = ' ' : run (ind - 1)
    diff (C p _ s _) =
      if s - p > 0
        then s - p
        else 0

catchup :: (Output String m Context, Monad m) => Writer m Context
catchup = Writer (WA $ \c -> write (run . diff $ c) c) mempty
  where
    run 0 = ""
    run ind = ' ' : run (ind - 1)
    val (C _ _ _ s) = unpack . stackPeek $ s
    unpack (Just a) = pure a
    diff c@(C p _ _ _) =
      if val c - p > 0
        then val c - p
        else 0

(<->) ::
     (Output String m Context, Monad m) => Writer m Context -> Writer m Context -> Writer m Context
x <-> y = x <> string " " <> y

-- TODO to do this correctly and have per line justifications and stuff I need to sum across each line how far its already gone (need to know the length of each string)
writeEquation :: (Output String m Context, Monad m) => EQUATION -> Writer m Context
writeEquation (VAL x) = string . unpack $ x
writeEquation (EQU x y) = writeEquation x <-> string "=" <-> writeEquation y
writeEquation (AND x y) = writeEquation x <> newline <> string "AND" <-> writeEquation y
-- data Context = Context
--   { pos :: Pointer
--   , ind :: Pointer
--   , just :: Pointer
--   } deriving (Eq, Show)
-- 
-- linPos :: Context -> Pointer
-- linPos c = pos c <> ind c
-- 
-- instance Semigroup Context where
--   (Context p1 i1 j1) <> (Context p2 i2 j2) = Context (p1 <> p2) (i1 <> i2) (j1 <> j2)
-- 
-- instance Monoid Context where
--   mempty = Context mempty mempty mempty
-- 
-- type Pointer = Sum Int
-- 
-- type Out = String
-- 
-- newtype WriteAction m a = WA
--   { runWriteAction :: (Output Out m) =>
--                         a -> m ()
--   }
-- 
-- instance (Monad m) => Semigroup (WriteAction m a) where
--   (WA w1) <> (WA w2) = WA $ \c -> w1 c >> w2 c
-- 
-- instance (Monad m) => Monoid (WriteAction m a) where
--   mempty = WA $ const (pure mempty)
-- 
-- newtype ContextualWriter m a = Writer
--   { runWriter :: (WriteAction m a, a)
--   }
-- 
-- instance (Monad m, Monoid a) => Semigroup (ContextualWriter m a) where
--   (Writer (w1, n1)) <> (Writer (w2, n2)) = Writer (w1 <> (n1 · w2), n1 <> n2)
--     where
--       n · (WA w) = WA $ \c -> w (n <> c)
-- 
-- instance (Monad m, Monoid a) => Monoid (ContextualWriter m a) where
--   mempty = Writer (mempty, mempty)
-- 
-- type Writer m
--    = (Monad m) =>
--        ContextualWriter m Context
-- 
-- class (Monad m) =>
--       Output w m | m -> w where
--   write :: w -> m ()
-- 
-- instance Output String IO where
--   write = print
-- 
-- writer :: (Context -> Out) -> Int -> Writer m
-- writer f s = Writer (WA $ write . f, Context (pure s) 0 0)
-- 
-- string :: String -> Writer m
-- string s = writer (const s) (length s)
-- 
-- indent :: Writer m -> Writer m
-- indent w = Writer (mempty, Context 0 4 0) <> w <> Writer (mempty, Context 0 (-4) 0)
-- 
-- justify :: Int -> Writer m -> Writer m
-- justify i w = Writer (mempty, Context 0 0 (pure i)) <> w <> Writer (mempty, Context 0 0 (pure (-i)))
-- 
-- (<->) :: Writer m -> Writer m -> Writer m
-- x <-> y = x <> string " " <> y
-- 
-- newline :: Writer m
-- newline = Writer (WA $ write . \c -> '\n' : run (ind c), Context (pure 1) mempty mempty)
--   where
--     run 0 = ""
--     run ind = ' ' : run (ind - 1)
-- 
-- justification :: Writer m
-- justification = Writer (WA $ write . \c -> run $ linPos c - just c, mempty)
--   where
--     run 0 = ""
--     run ind = ' ' : run (ind - 1)
-- 
-- -- TODO to do this correctly and have per line justifications and stuff I need to sum across each line how far its already gone (need to know the length of each string)
-- writeEquation :: EQUATION -> Writer m
-- writeEquation (VAL x) = string . unpack $ x
-- writeEquation (EQU x y) = justification <> writeEquation x <-> string "=" <-> writeEquation y
-- writeEquation (AND x y) = writeEquation x <> newline <> string "AND" <-> writeEquation y
