
module Parser where
import           Control.Applicative              hiding (many)

import           Term
import           Text.Parsec                      hiding ((<|>))
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token                as Token
import           Unbound.Generics.LocallyNameless



lambda' :: String -> Term -> Term
lambda' x t = Lambda $ bind (string2Name x) t

var' :: String -> Term
var' = Var . string2Name

tokenizer :: Token.TokenParser ()
tokenizer = Token.makeTokenParser style
  where ops = ["->","\\","="]
        style = haskellStyle {Token.reservedOpNames = ops }


ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer

definition :: Parser Definition
definition = do
  name <- ident
  reservedOp "="
  body <- term
  return . Definition . bind (string2Name name) $ body

var :: Parser Term
var = do
  name <- ident
  return $ var' name

data' :: Parser Term
data' = do
    reserved "Data"
    return Data



term :: Parser Term
term = parens app <|> lambda <|> data' <|> var

lambda :: Parser Term
lambda = do
    reservedOp "\\"
    name <- ident
    reservedOp "->"
    body <- term
    return . Lambda . bind (string2Name name) $ body

app :: Parser Term
app = do
  ts <- many1 term
  return . foldl1 App $ ts

parseTerm :: String -> Term
parseTerm input =
  case parse app "<stdin>" input of
    Left err  -> error (show err)
    Right ast -> ast

parseModule :: String -> Module
parseModule input =
    case parse (many1 definition) "<stdin>" input of
      Left err  -> error (show err)
      Right ast -> Module ast
