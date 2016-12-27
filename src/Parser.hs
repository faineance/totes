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

lexer = haskellStyle
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer


definition = do
  name <- ident
  reservedOp "="
  val <- term
  return (Def (string2Name name) val)

signature = do
  name <- ident
  reservedOp ":"
  val <- term
  return (Sig (string2Name name) val)

declaration :: Parser Declaration
declaration = definition <|> signature




var :: Parser Term
var = do
  name <- ident
  return $ var' name

term = lambda <|> var <|> app

lambda :: Parser Term
lambda = do
    reservedOp "\\"
    name <- ident
    reservedOp "->"
    body <- term
    return . Lambda . bind (string2Name name) $ body

app :: Parser Term
app = do
  ts <- many term
  return . foldl1 App $ ts


parseModule :: String -> Module
parseModule input =
    case parse (many1 declaration) "<stdin>" input of
      Left err  -> error (show err)
      Right ast -> ast
