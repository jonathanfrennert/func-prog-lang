{-|
Module      : Lang.Syntax
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Lang.Syntax (
  -- * Type
  Expr (..),
  -- * Core expressions
  Name,
  CoreExpr,
  isAtomicExpr,
  -- ** Definitions
  CoreDefn,
  -- ** Case alternatives
  Alter,
  CoreAlt,
  -- ** Let(rec)
  IsRec,
  recursive,
  nonRecursive,
  -- ** Programs
  Program,
  CoreProgram,
  -- ** Supercombinators
  ScDefn,
  CoreScDefn,
  ) where

-- | All the possible expressions in the language.
data Expr a
  = EVar Name                 -- ^ Variables
  | ENum Int                  -- ^ Numbers
  | EConstr Int Int           -- ^ Constructor tag arity
  | EAp (Expr a) (Expr a)     -- ^ Application
  | ELet                      -- ^ Let(rec) expressions
      IsRec                   -- ^ True\/False = recursive\/non-recursive
      [(a, Expr a)]           -- ^ Definitions
      (Expr a)                -- ^ Body of let(rec)
  | ECase                     -- ^ Case expressions
      (Expr a)                -- ^ Expression to scrutinise
      [Alter a]               -- ^ Alternatives
  | ELam [a] (Expr a)         -- ^ Lambda abstractions
  deriving(Show, Eq)

type Name = String

-- | Core expression are made up by strings.
type CoreExpr = Expr Name

-- | Definitions have a name and an expression.
type CoreDefn = (Name, CoreExpr)

-- | Case alternatives have a tag, arguments and an expression.
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- | Let expressions are identified as either recursive or not.
type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive     = True
nonRecursive  = False

-- | A program is a list of supercombinators.
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- | A supercombinator has a name, arguments and and an expression.
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- | True if the expression is a variable or number, otherwise false.
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False
