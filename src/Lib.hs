{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Applicative hiding (many)
import Control.Monad (mfilter)
import Data.Char
import Data.Functor
import Data.List.NonEmpty
import Data.Maybe

import Prelude as P

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead [] = Nothing

--toList :: NonEmpty a -> [a]
--toList (head :| tail) = head : tail
fromList :: [a] -> Maybe (NonEmpty a)
fromList [] = Nothing
fromList (x:xs) = Just $ x :| xs

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f parser = Parser {runParser = fmap (first f) . runParser parser}

instance Applicative Parser where
  pure a = Parser $ \str -> Just (a, str)
  fp <*> ap =
    Parser
      { runParser =
          \str -> do
            (f, rest) <- runParser fp str
            (a, rest') <- runParser ap rest
            return (f a, rest')
      }

instance Monad Parser where
  return = pure
  p >>= f =
    Parser $ \str -> do
      (a, rest) <- runParser p str
      runParser (f a) rest

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \str -> runParser p1 str <|> runParser p2 str

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser {runParser = \str -> fmap (, P.tail str) $ mfilter f $ maybeHead str}

symbol :: Char -> Parser Char
symbol c = satisfy (== c)

boolParser :: Parser Bool
boolParser = symbol 't' $> True <|> symbol 'f' $> False

digitParser :: Parser Char
digitParser = satisfy isDigit

readInt :: String -> Int
readInt str = read str :: Int

intParser :: Parser Int
intParser = readInt . toList <$> many1 digitParser

many :: Parser a -> Parser [a]
many p = many'
  where
    many' = some <|> pure []
    some = (:) <$> p <*> many'

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = (:|) <$> p <*> many p
