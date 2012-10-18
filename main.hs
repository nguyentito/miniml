import Control.Applicative
import Text.Parsec
import Text.Parsec.String

data MLOp = Add | Sub | Mul | Div | Fst | Snd

nameOp Add = "+"
nameOp Sub = "-"
nameOp Mul = "*"
nameOp Div = "/"
nameOp Fst = "fst"
nameOp Snd = "snd"

data MLExpr = MLInt Integer
            | MLBool Bool
            | MLPair MLExpr MLExpr
            | MLVar String
            | MLPrim MLOp
            | MLLet { letVar :: MLVar, letValue :: MLExpr, letBody :: MLExpr }
            | MLFun { funArg :: MLVar, funBody :: MLExpr }
            | MLApp MLExpr MLExpr

mlExpr = mlConst <|> mlPair <|> mlLet <|> mlVar <|> mlPrim <|> mlFun <|> mlApp

mlConst = ( (MLInt . read) <$> many1 digit )
      <|> ( MLBool True <$ string "true" )
          ( MLBool False <$ string "false" )
                    
mlPair = do
  char '('
  fst <- mlExpr
  char ','
  snd <- mlExpr
  char ')'
  pure (MLPair fst snd)

mlLet = do
  string "let"
  many1 space
  id <- mlVar
  spaces
  string "="
  spaces
  val <- mlExpr
  many1 space
  string "in"
  many1 space
  body <- mlExpr
  pure (MLLet { letVar = id, letValue = val, letBody = body })
  
mlVar = (:) <$> letter <*> many (char '_' <|> alphaNum)

mlFun = do
  string "fun"
  many1 space
  id <- mlVar
  spaces
  string "->"
  spaces
  body <- mlExpr
  pure (MLFun { funArg = id, funBody = body })

alternatives = foldr1 (<|>)

mlPrim = alternatives (map f [Add,Sub,Mul,Div,Fst,Snd])
  where f x = MLOp x <$ string (nameOp x)

mlApp 
  

miniML = mlExpr

main = do
  result <- parseFromFile miniML test.ml
  case result of
    Left err -> putStrLn ("Erreur : " ++ err)
    Right _ -> putStrLn "Succès"
    