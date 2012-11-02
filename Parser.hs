import Control.Applicative
import Text.Parsec
import Text.Parsec.String

import Ast

junk = spaces -- this will eventually include comments as well
token x = x <* junk -- parse all the non-significant characters following the token
                    -- and throw them away

mlExpr = mlConst <|> mlPair <|> mlLet <|> mlVar <|> mlPrim <|> mlFun <|> mlApp

mlConst = ( (MLInt . read) <$> many1 digit )
      <|> ( MLBool True  <$ string "true"  )
          ( MLBool False <$ string "false" )
                    
mlPair = between (char '(') (char ')') $
                 MLPair <$> mlExpr <*> (char ',' *> mlExpr)

mlLet = do
  (id, val) <- letBinding
  body <- mlExpr
  pure (MLLet { letVar = id, letValue = val, letBody = body })  
  where
    letBinding = between (string "let" *> many1 space)
                         (many1 space *> string "in" *> many1 space) $
                         (,) <$> mlVar <*> (equalSign *> mlExpr)
    equalSign = spaces *> char '=' *> spaces
      
-- note : en fait il n'y a pas forcément d'espace après "let ... in", 
--        il peut tout autant y avoir une parenthèse

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

mlPrim = choice (map f [Add,Sub,Mul,Div,Fst,Snd])
  where f x = MLOp x <$ string (nameOp x)

mlApp = undefined

miniMLParser :: Parser MLExpr
miniMLParser = mlExpr

