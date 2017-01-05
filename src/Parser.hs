
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


tvar' :: String -> Type
tvar' = TVar . string2Name


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

var,term,lambda :: Parser Term
var = do
  name <- ident
  return $ var' name

--
-- base :: Parser Term
-- base = (reserved "Data" >> return (Base Data)) <|> (reserved "Codata" >> return (Base Codata))

term = lambda <|> (reserved "Data" >> return (Data)) <|> var

lambda = do
    reservedOp "\\"
    name <- ident
    reservedOp "->"
    body <- term
    return . Lambda . bind (string2Name name) $ body

-- app :: Parser Term
-- app = do
--   ts <- many1 term
--   return . foldl1 App $ ts

tvar, tarrow :: Parser Type
tvar = do
  name <- ident
  return $ tvar' name
tarrow = do
  reservedOp "\\"
  t1 <- ty
  reservedOp "->"
  t2 <- ty
  return $ TArr t1 t2

ty :: Parser Type
ty = tvar <|> tarrow

parseTerm :: String -> Term
parseTerm = parseP term

parseType :: String -> Type
parseType = parseP ty

parseP:: Parser a -> String -> a
parseP p input =
  case parse p "<stdin>" input of
    Left err  -> error (show err)
    Right ast -> ast
