module Main where

import Ast
import Parser

main = do
  result <- miniMLParseFile "test.ml"
  case result of
    Left err -> putStrLn "Erreur : " >> print err
    Right expr -> print expr
