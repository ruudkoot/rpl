{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- | Syntax

type Name = String

data Module a = Module Name [Binding a]

data Binding a
    = TERM Name Kind Type (Term a)
    | TYPE Name Kind Type

data Kind
    = STAR

instance Show Kind where
    show (STAR       ) = "⋆"

data Type
    = VAR    Name
    | UNIT
    | PROD   Type Type
    | SUM    Type Type
    | ARROW  Type Type
    | FIX    Name Type
    | FORALL Name Type
    | EXISTS Name Type

instance Show Type where
    show (VAR    α    ) = α
    show (UNIT        ) = "◇"
    show (PROD   τ₁ τ₂) = "(" ++ show τ₁ ++ " ⨉ " ++ show τ₂ ++ ")"
    show (SUM    τ₁ τ₂) = "(" ++ show τ₁ ++ " + " ++ show τ₂ ++ ")"
    show (ARROW  τ₁ τ₂) =        show τ₁ ++ " → " ++ show τ₂
    show (FIX    α  τ ) = "(μ" ++ α ++ "." ++ show τ ++ ")"
    show (FORALL α  τ ) = "(∀" ++ α ++ "." ++ show τ ++ ")"
    show (EXISTS α  τ ) = "(∃" ++ α ++ "." ++ show τ ++ ")"

data Term a
    = Var    a Name
    | App    a (Term a) (Term a)
    | Abs    a Name (Term a)
    | Fix    a Name (Term a)
    | Unit   a
    | Pair   a (Term a) (Term a)
    | Inl    a (Term a)
    | Inr    a (Term a)
    | Fold
    | Unfold
    | TyAbs    Name     (Term a)
    | TyApp    (Term a) (Term a)
    
-- | Examples

typeEx1 = FORALL "b" (FIX "a" (PROD UNIT (VAR "a") `ARROW` SUM (VAR "b") UNIT))
