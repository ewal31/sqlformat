{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  FunctionalDependencies, RankNTypes #-}

module Printer.SQL where

import Data.Monoid (Sum(..))

type Indent = Int

newtype WriteAction m = WA
  { runWriteAction :: (Output String m) =>
                        Indent -> m ()
  }

instance (Monad m) => Semigroup (WriteAction m) where
  (WA w1) <> (WA w2) = WA $ \indent -> w1 indent >> w2 indent

instance (Monad m) => Monoid (WriteAction m) where
  mempty = WA $ const (pure mempty)

newtype Writer m = Writer
  { runWriter :: (WriteAction m, Sum Indent)
  }

instance (Monad m) => Semigroup (Writer m) where
  (Writer (w1, n1)) <> (Writer (w2, n2)) = Writer (w1 <> (n1 · w2), n1 <> n2)
    where
      (Sum n) · (WA w) = WA $ \indent -> w (indent + n)

instance (Monad m) => Monoid (Writer m) where
  mempty = Writer (mempty, mempty)

class (Monad m) =>
      Output w m | m -> w where
  write :: w -> m ()

instance Output String IO where
  write = print

newline' :: WriteAction m
newline' = WA $ \_ -> write "\n"

newline :: Writer m
newline = Writer (newline', mempty)

indent :: (Monad m) => Writer m
indent = Writer (mempty, pure 4)

undent :: (Monad m) => Writer m
undent = Writer (mempty, pure (-4))

writeConst' :: WriteAction m
writeConst' = WA $ \ind -> write $ run ind
  where
    run 0 = "const"
    run ind = ' ' : run (ind - 1)

writeConst :: Writer m
writeConst = Writer (writeConst', mempty)
