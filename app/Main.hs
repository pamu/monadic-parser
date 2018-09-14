module Main where

import AST (evaluate)
import Control.Monad
import Parser
import Parsers (expr)

main :: IO ()
main =
  forever $ do
    line <- getLine
    putStrLn $ show $ process line

process :: String -> String
process str =
  case runParser expr str of
    Nothing -> "Syntax error"
    Just (result, _) -> "Result: " ++ (show $ evaluate result)
