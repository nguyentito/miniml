module Parser (miniMLParser,
               miniMLParseFile) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token)
import Text.Parsec.String

import Ast


--- External interface ---

miniMLParser :: Parser MLExpr
miniMLParser = junk *> mlExpr -- remove spaces/comments at the start
                              -- (see next section)
               
miniMLParseFile = parseFromFile (miniMLParser <* eof)


--- Lexical utilities (no need for a separate lexer) ---

junk = spaces -- this will eventually include comments as well
token x = x <* junk -- parse all the non-significant characters following the token
                    -- and throw them away
          
identChar = alphaNum <|> char '_'
keyword x = token . try $ string x *> notFollowedBy identChar
punctuation = token . char


--- The actual language grammar (syntactic analysis) ---

-- To avoid infinite left recursion,
-- the <application> = <expr1> <expr2> production rule is treated using chainl1

mlExpr = mlNonArg <|> mlApp

mlApp = mlArgExpr `chainl1` (pure MLApp)
mlNonArg = mlFun <|> mlLet 
mlArgExpr = mlParenSubExpr <|> mlConst <|> mlPrim <|> mlVar

-- Something between parentheses can be either a pair or just a normal expression
mlParenSubExpr = between (punctuation '(') (punctuation ')') inside
  where inside = mlExpr >>= (\x -> pair x <|> single x)
        single expr = pure expr
        pair first = do
          punctuation ','
          second <- mlExpr
          pure $ MLPair first second
          
-- Easy to parse constructs : const, prim, var, let, fun

mlConst = ( (MLInt . read) <$> token numericLiteral )
      <|> ( MLBool True  <$ keyword "true"  )
      <|> ( MLBool False <$ keyword "false" )
  where numericLiteral = many1 digit <* notFollowedBy identChar
                    
mlPrim = choice (map f [Add,Sub,Mul,Div,Fst,Snd,OpIf,OpFix])
  where f x = MLPrim x <$ keyword (nameOp x)

-- notFollowedBy used to implement the longest matching token rule
mlIdent = token $ do
  ident <- (:) <$> letter <*> many identChar
  notFollowedBy identChar
  pure $ MLIdent ident
mlVar = MLVar <$> mlIdent

mlLet = do
  keyword "let"
  ident <- mlIdent
  punctuation '='
  val <- mlExpr
  keyword "in"
  body <- mlExpr
  pure (MLLet { letVar = ident, letValue = val, letBody = body })  

mlFun = do
  keyword "fun"
  ident <- mlIdent
  token (string "->")
  body <- mlExpr
  pure (MLFun { funArg = ident, funBody = body })

