{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  FunctionalDependencies, RankNTypes #-}

module Printer.SQL where

import AST.Equation
import Data.ByteString.Char8 (unpack)
import Data.Monoid (Sum(..))

type Context = Sum Int

type Out = String

newtype WriteAction m a = WA
  { runWriteAction :: (Output Out m) =>
                        a -> m ()
  }

instance (Monad m) => Semigroup (WriteAction m a) where
  (WA w1) <> (WA w2) = WA $ \c -> w1 c >> w2 c

instance (Monad m) => Monoid (WriteAction m a) where
  mempty = WA $ const (pure mempty)

newtype ContextualWriter m a = Writer
  { runWriter :: (WriteAction m a, a)
  }

instance (Monad m, Monoid a) => Semigroup (ContextualWriter m a) where
  (Writer (w1, n1)) <> (Writer (w2, n2)) = Writer (w1 <> (n1 · w2), n1 <> n2)
    where
      n · (WA w) = WA $ \c -> w (n <> c)

instance (Monad m, Monoid a) => Monoid (ContextualWriter m a) where
  mempty = Writer (mempty, mempty)

type Writer m
   = (Monad m) =>
       ContextualWriter m Context

writer :: (Context -> Out) -> Writer m
writer f = Writer (WA $ write . f, mempty)

string :: String -> Writer m
string s = writer $ \ind -> run ind
  where
    run 0 = s
    run ind = ' ' : run (ind - 1)

class (Monad m) =>
      Output w m | m -> w where
  write :: w -> m ()

instance Output String IO where
  write = print

indent :: Writer m
indent = Writer (mempty, pure 4)

undent :: Writer m
undent = Writer (mempty, pure (-4))

indent' :: Writer m -> Writer m
indent' p = indent <> p <> undent

(<->) :: Writer m -> Writer m -> Writer m
x <-> y = x <> string " " <> y

newline :: Writer m
newline = string "\n"

writeConst :: Writer m
writeConst = string "const"

newline' :: WriteAction m a
newline' = WA $ \_ -> write "\n"

writeConst' :: (Num a, Eq a) => WriteAction m a
writeConst' = WA $ \ind -> write $ run ind
  where
    run 0 = "const"
    run ind = ' ' : run (ind - 1)

writeEquation :: EQUATION -> Writer m
writeEquation (VAL x) = string . unpack $ x
writeEquation (EQU x y) = writeEquation x <-> string "=" <-> writeEquation y
writeEquation (AND x y) = writeEquation x <-> string "AND" <-> writeEquation y
