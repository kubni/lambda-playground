{-# LANGUAGE FlexibleContexts #-}
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



runParser str = Parsec.runParser (expr <* eof) () "" str


expr :: (Stream s m Char) => ParsecT s u m Expr
expr = chainl1 (nonApp <* spaces) (pure App) <?> "expr" where -- TODO: How does chainl1 work?
  name = do
    head <- letter
    tail <- many (letter <|> digit <|> char '\'')
    (return (head:tail)) <?> "name"
  var = (Var <$> name) <?> "var"
  abs = do
    names <- char '\\' >> spaces >> sepEndBy1 name spaces  -- \\ is used in place of λ for easier writing
    expression <- char '.' >> spaces >> expr
    (return (foldl1 (.) (fmap Abs names) expression)) <?> "abs"
  nonApp = (parens expr <|> abs <|> var) <?> "non-app expr"
  parens = between
    (char '(' >> spaces)
    (char ')' >> spaces)


-- name parses the alphanumeric string

-- Here is what I think the return line in the abs parser is doing:
-- Lets say that we had \x y z.x+y+z
-- names = [x, y, z]
-- Then, we partially apply Abs constructor by passing names to it, so we have [Abs x _, Abs y _, Abs z _]
-- We use a fold to apply dot operator to those constructors, practically nesting them:
   --      Abs "x" . Abs "y" . Abs "z"
   -- which is a function equivalent to this:
   --  \e -> Abs "x" (Abs "y" (Abs "z" e))
-- At the end we just lift that into a Parsec monad type, while the <?> is there for errors, when our parser fails and doesn't consume anything


-- chainl1 uses (pure App) as an operator, and the result of (nonApp <* spaces) as a operands.
-- If (nonApp <* spaces parses 2 Abs-es, chainl1 will create App Abs1 Abs2)
-- TODO: Blog mentions App not being in expr because of a potentional infinite loop? How does that happen?





main :: IO ()
main = do
  let expr2 = App (Abs "x" (Var "x")) (Var "y")
  let mainExpr = Abs "x" (App (Abs "y" (Var "y")) expr2)
  putStrLn $ encode mainExpr
