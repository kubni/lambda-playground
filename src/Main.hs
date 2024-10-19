module Main where

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, runParser)




type Name = String

-- Example abstraction: λx.x
-- Example application: (λx.x) x

data Expr = Var Name
            | Abs Name Expr
            | App Expr Expr
            deriving (Eq, Show)

encode :: Expr -> String
encode = enc' where
  enc' (Var name) = name
  enc' (Abs name body) = "λ" ++ name ++ "." ++ enc' body
  enc' (App expr1 expr2) = "(" ++ enc' expr1 ++ ")" ++ enc' expr2












main :: IO ()
main = do
  let expr2 = App (Abs "x" (Var "x")) (Var "y")
  let mainExpr = Abs "x" (App (Abs "y" (Var "y")) expr2)
  putStrLn $ encode mainExpr
