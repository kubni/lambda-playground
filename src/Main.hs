module Main where

type Name = String

-- TODO: GADTs
-- data Expr where
--   TmVar :: VarName -> Expr
--   TmAbs :: VarName -> Expr -> Expr
--   TmApp :: Expr -> Expr -> Expr
--   deriving (Eq, Show)


-- Example abstraction: λx.x
-- Example application: (λx.x) x

data Expr = Var Name
            | Abs Name Expr
            | App Expr Expr
            deriving (Eq, Show)

encode :: Expr -> String
encode = go where
  go (Var name) = name
  go (Abs name body) = "λ" ++ name ++ "." ++ go body
  go (App expr1 expr2) = "(" ++ go expr1 ++ go expr2 ++ ")"












main :: IO ()
main = putStrLn "Hello, Haskell!"
