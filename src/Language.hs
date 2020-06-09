module Language where

data Expr a
  = EVar Name                 -- ^ Variables
  | ENum Int                  -- ^ Numbers
  | EConstr Int Int           -- ^ Constructor tag arity
  | EAp (Expr a) (Expr a)     -- ^ Application
  | ELet                      -- ^ Let(rec) expressions
      IsRec                   -- ^ True/False = recursive/non-recursive
      [(a, Expr a)]           -- ^ Definitions
      (Expr a)                -- ^ Body of let(rec)
  | ECase                     -- ^ Case expressions
      (Expr a)                -- ^ Expression to scrutinise
      [Alter a]               -- ^ Alternatives
  | ELam [a] (Expr a)         -- ^ Lambda abstractions
  deriving(Show)

type Name = String

type CoreExpr = Expr Name

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive     = True
nonRecursive  = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- | Helper methods

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False
