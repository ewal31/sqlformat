{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies, FlexibleInstances,
  UndecidableInstances, IncoherentInstances, FunctionalDependencies
  #-}

module Tuple where

type family TupleFn ty out where
  TupleFn () output = output
  TupleFn (a, b) output = a -> TupleFn b output

class ApplyFnToTuple a out where
  applyFnToTuple :: a -> TupleFn a out -> out

instance ApplyFnToTuple b out => ApplyFnToTuple (a, b) out where
  applyFnToTuple (a, b) fn = applyFnToTuple b (fn a)

instance ApplyFnToTuple () out where
  applyFnToTuple _ fn = fn

tstapplytuplefn :: String
tstapplytuplefn = applyFnToTuple ("hello", ('y', ('s', ()))) $ \a b c -> a ++ [b, c]

-- https://stackoverflow.com/questions/9656797/variadic-compose-function
class Comp a b c | a b -> c where
  (...) :: a -> b -> c

instance (a ~ c, r ~ b) => Comp (a -> b) c r where
  f ... g = f g

instance (Comp (a -> b) d r1, r ~ (c -> r1)) => Comp (a -> b) (c -> d) r where
  f ... g = \c -> f ... g c
