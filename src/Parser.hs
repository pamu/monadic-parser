module Parser where

import Control.Applicative hiding (many)
import Control.Monad (mfilter)
import Data.Bifunctor
import Data.Char
import Data.Functor
import Data.List.NonEmpty
import Data.Maybe

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

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
