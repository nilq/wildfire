module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr  as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating =
  Float <$> float

binary s assoc =
  Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops =
  [ [ binary "*" Ex.AssocLeft
    , binary "/" Ex.AssocLeft
    ]
    ,
    [ binary "+" Ex.AssocLeft
    , binary "-" Ex.AssocLeft
    ]
    ,
    [ binary "<" Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr =
  Ex.buildExpressionParser binops factor

function :: Parser Expr
function = do
  reserved "func"
  name <- identifier
  args <- (many identifier) colon
  body <- many (expr semiSep)

  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "summon"
  name <- identifier
  args <- many identifier

  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- commaSep expr

  return $ Call name args

factor :: Parser Expr
factor =
      try floating
  <|> try int
  <|> try call
  <|> try variable
  <|> ifthen
  <|> for
  <|> (parens expr)

defn :: Parser Expr
defn = try extern
  <|> try function
  <|> expr


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  -- reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s =
  parse (contents expr) "<you fucked up>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s =
  parse (contents toplevel) "<you fucked up>" s
