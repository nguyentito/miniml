module Main where

import qualified Text.Parsec.String as TPS

import Ast
import Parser

main = do
  result <- TPS.parseFromFile miniMLParser "test.ml"
  case result of
    Left err -> putStrLn "Erreur : " >> print err
    Right expr -> print expr
