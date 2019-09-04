{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  FunctionalDependencies, RankNTypes #-}

module Printer.SQL where

import AST.Equation
import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.ByteString.Char8 (unpack)
import Data.Monoid (Sum(..))

type Written = String

data Context = C
  { pos :: Sum Int
  , ind :: Sum Int
  , set1 :: Sum Int
  }

fromPos :: Sum Int -> Context
fromPos p = C p mempty mempty

fromInd :: Sum Int -> Context
fromInd i = C mempty i mempty

fromSet1 :: Sum Int -> Context
fromSet1 = C mempty mempty

instance Semigroup Context where
  (C p1 i1 s1) <> (C p2 i2 s2) = C (p1 <> p2) (i1 <> i2) (s1 <> s2)

instance Monoid Context where
  mempty = C mempty mempty mempty

newtype Writer m a = W
  { runWriter :: (Monad m) =>
                   a -> m a
  }

instance (Monad m, Monoid a) => Semigroup (Writer m a) where
  (W w1) <> (W w2) = W $ w1 >=> w2

instance (Monad m, Monoid a) => Monoid (Writer m a) where
  mempty = W pure

class (Monad m) =>
      Output w m b | m -> w where
  write :: w -> b -> m b

instance Output Written IO Context where
  write s c = do
    print s
    return $ c <> (fromPos . pure . length $ s)

string :: (Output String m a, Monoid a) => Written -> Writer m a
string s = W $ write s

indent :: (Monad m) => Writer m Context -> Writer m Context
indent w = (W $ pure . i) <> w <> (W $ pure . u)
  where
    i = liftA2 (<>) id (fromInd . (+ 4) . ind)
    u = liftA2 (<>) id (fromInd . ((-) 4) . ind)

set ::
     (Monad m)
  => (Context -> Sum Int)
  -> (Sum Int -> Context)
  -> Writer m Context
  -> Writer m Context
set g h w =
  W $ \c -> do
    let diff = liftA2 (-) pos g c
    runWriter ((W $ pure . i diff) <> w <> (W $ pure . u diff)) c
  where
    i diff = liftA2 (<>) id (h . const diff)
    u diff = liftA2 (<>) id (h . const (negate diff))

newline :: (Output String m Context, Monad m) => Writer m Context
newline = W $ \c -> write (('\n' :) . run . ind $ c) (reset c)
  where
    run 0 = ""
    run i = ' ' : run (i - 1)
    reset = liftA2 (<>) id (fromPos . ((-1) -) . pos) -- subtract extra 1 for \n

catchup :: (Output String m Context, Monad m) => (Context -> Sum Int) -> Writer m Context
catchup g = W $ \c -> write (run . liftA2 diff g pos $ c) c
  where
    run 0 = ""
    run ind = ' ' : run (ind - 1)
    diff s p =
      if s - p > 0
        then s - p
        else 0

(<->) ::
     (Output String m Context, Monad m) => Writer m Context -> Writer m Context -> Writer m Context
x <-> y = x <> string " " <> y

writeEquation :: (Output String m Context, Monad m) => EQUATION -> Writer m Context
writeEquation (VAL x) = string . unpack $ x
writeEquation (EQU x y) = catchup set1 <> writeEquation x <-> string "=" <-> writeEquation y
writeEquation (PLUS x y) = writeEquation x <-> string "+" <-> writeEquation y
writeEquation (MINUS x y) = writeEquation x <-> string "-" <-> writeEquation y
writeEquation (TIMES x y) = writeEquation x <-> string "*" <-> writeEquation y
writeEquation (DIV x y) = writeEquation x <-> string "/" <-> writeEquation y
writeEquation (BRACKETS e) = string "(" <-> writeEquation e <-> string ")"
writeEquation (AND x y) = writeEquation x <> newline <> string "AND" <-> writeEquation y
writeEquation (OR x y) = writeEquation x <-> string "OR" <-> writeEquation y
writeEquation (NOT e) = undefined
writeEquation (IS x y) = writeEquation x <-> string "IS" <-> writeEquation y
writeEquation (LESS x y) = writeEquation x <-> string "<" <-> writeEquation y
writeEquation (GREAT x y) = writeEquation x <-> string ">" <-> writeEquation y
writeEquation (NEQ x y) = writeEquation x <-> string "<>" <-> writeEquation y
writeEquation (LESSEQ x y) = writeEquation x <-> string "<=" <-> writeEquation y
writeEquation (GREATEQ x y) = writeEquation x <-> string ">=" <-> writeEquation y
writeEquation (FUNC name eqs) = undefined
writeEquation (CASE (Just e1) whenthens (Just e2)) = undefined
writeEquation (CASE (Just e1) whenthens Nothing) = undefined
writeEquation (CASE Nothing whenthens (Just e2)) = undefined
writeEquation (CASE Nothing whenthens Nothing) = undefined
writeEquation (S_EXP s) = undefined
