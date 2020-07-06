{-|
Module      : Lang.StdPrelude
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Lang.StdPrelude (
  preludeDefs,
  -- * SKI Combinators
  i,
  k,
  k1,
  s,
  -- * Composition functions
  compose,
  twice
  ) where

import Lang.Syntax

-- | A bunch of useful functions (see below).
preludeDefs :: CoreProgram
preludeDefs = [i, k, k1, s, compose, twice]

-- | The identity operator __I__ takes one argument and returns it.
i :: ScDefn Name
i = ("I", ["x"], EVar "x")

-- | __K__ When applied to any argument x, yields one-argument constant function
-- __K__x,which when applied to any argument returns x.
k :: ScDefn Name
k = ("K", ["x","y"], EVar "x")

-- | __K1__ When applied to any argument x, yields one-argument constant function
-- __K1__x, which when applied to any argument, y, always returns y.
k1 :: ScDefn Name
k1 = ("K1",["x","y"], EVar "y")

-- | The substitution operator __S__ takes three arguments. It applies the first
-- argument to the third argument, which is then applied to the result of the
-- second argument applied to the third argument.
s :: ScDefn Name
s = ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))

-- | The composition operator takes three arguments. The first argument
-- is applied to the second argument, which is applied to the third argument.
compose :: ScDefn Name
compose = ("compose", ["f","g","x"], EAp (EVar "f")
                                    (EAp (EVar "g") (EVar "x")))

-- | The twice operator takes one argument. When applied to any function
-- argument f, yields a function which takes a single argument and applies
-- f to the argument twice using 'compose'.
twice :: ScDefn Name
twice = ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
