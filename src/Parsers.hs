module Parsers where

import AST
import Control.Applicative hiding (many, optional)
import Data.Char
import Data.Functor
import Data.List (foldl')
import Data.List.NonEmpty
import ExtendedPrelude (readInt)
import Parser
import ParserBuilders

digit :: Parser Char
digit = satisfy isDigit

int :: Parser Int
int = readInt . toList <$> many1 digit

char :: Char -> Parser Char
char = symbol

anyChar :: Parser Char
anyChar = satisfy isAlphaNum

string :: String -> Parser String
string str = combineParsers $ foldl' (\r c -> char c : r) [] str

anyString :: Parser String
anyString = toList <$> many1 anyChar

separator :: Parser String
separator = many $ oneOf $ fromList " \n\r"

token :: String -> Parser String
token str = do
  separator
  word <- string str
  separator
  return word

parensEnclosed :: Parser a -> Parser a
parensEnclosed p = do
  token "("
  insideParens <- p
  token ")"
  return insideParens

optional :: String -> Parser String
optional str = string str <|> pure ""

integer :: Parser Int
integer = do
  value <- optional "-"
  intValue <- int
  let result =
        if value == ""
          then intValue
          else -intValue
  return result

number :: Parser Expr
number = Number <$> integer

mulExpr :: Parser Expr
mulExpr = do
  a <- expr
  token "*"
  b <- expr
  return $ Mul a b

divExpr :: Parser Expr
divExpr = do
  a <- expr
  token "/"
  b <- expr
  return $ Div a b

addExpr :: Parser Expr
addExpr = do
  a <- expr
  token "+"
  b <- expr
  return $ Add a b

subExpr :: Parser Expr
subExpr = do
  a <- expr
  token "-"
  b <- expr
  return $ Sub a b

expr :: Parser Expr
expr =
  number <|> parensEnclosed divExpr <|> parensEnclosed mulExpr <|>
  parensEnclosed addExpr <|>
  parensEnclosed subExpr
