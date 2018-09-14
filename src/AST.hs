module AST
  ( Expr(..)
  , evaluate
  ) where

data Expr
  = Div Expr
        Expr
  | Mul Expr
        Expr
  | Add Expr
        Expr
  | Sub Expr
        Expr
  | Number Int
  deriving (Show)

evaluate :: Expr -> Int
evaluate (Number value) = value
evaluate (Div a b) = evaluate a `div` evaluate b
evaluate (Mul a b) = evaluate a * evaluate b
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
