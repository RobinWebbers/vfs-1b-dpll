module Parser
  ( prop
  , parse
  ) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Prop

-- The string parser we use in this file
type Parser = Parsec String ()

-- Variable declaration
var :: Parser (Prop String)
var = do
  v <- many1 letter
  return $ Lit v

-- Unary symbols (in this case negation)
unary :: Parser (Prop String)
unary = do
  _ <- oneOf "~!-¬"
  spaces
  p <- factor
  return $ Neg p

-- Parenthesized expression
paren :: Parser a -> Parser a
paren p = between (char '(') (char ')') p

-- A factor is either:
-- - paren expr
-- - unary
-- - var
factor :: Parser (Prop String)
factor = do
  spaces
  f <- paren expr <|> unary <|> var
  spaces
  return f

-- A term, in this case a conjunction
term :: Parser (Prop String)
term = do
  (t:ts) <- factor `sepBy1` (oneOf "&∧")
  spaces
  return $ foldl (:&:) t ts

-- An expression, in this case a disjunction
expr :: Parser (Prop String)
expr = do
  (e:es) <- term `sepBy1` (oneOf "|∨")
  spaces
  return $ foldl (:|:) e es

-- A full propositional expression, does not
-- accept garbage after the expression
prop :: Parser (Prop String)
prop = do
  e <- expr
  eof
  return e

parse :: String -> Prop String
parse raw = case Parsec.parse prop "haskell" raw of
  Right p -> p
  Left x -> error . show $ x
