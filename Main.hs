module Main where

main = do
  result <- Text.Parsec.parseFromFile miniML test.ml
  case result of
    Left err -> putStrLn ("Erreur : " ++ err)
    Right _ -> putStrLn "Succès"
