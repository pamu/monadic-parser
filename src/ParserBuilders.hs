{-# LANGUAGE TupleSections #-}

module ParserBuilders where

import Control.Applicative hiding (many)
import Control.Monad (mfilter)
import Data.Bifunctor
import Data.List (foldl')
import Data.List.NonEmpty
import ExtendedPrelude (maybeHead)
import Parser (Parser(..))
import Prelude as P

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser {runParser = \str -> fmap (, P.tail str) $ mfilter f $ maybeHead str}

symbol :: Char -> Parser Char
symbol c = satisfy (== c)

many :: Parser a -> Parser [a]
many p = many'
  where
    many' = some <|> pure []
    some = (:) <$> p <*> many'

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = (:|) <$> p <*> many p

combineParsers :: [Parser a] -> Parser [a]
combineParsers = foldl' (\r c -> (:) <$> c <*> r) (pure [])

oneOf :: NonEmpty Char -> Parser Char
oneOf (head :| tail) = foldl' (\r c -> r <|> symbol c) (symbol head) tail
