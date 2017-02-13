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
  [
    [ binary "*" Ex.AssocLeft
    , binary "/" Ex.AssocLeft
    ]
    ,
    [ binary "+" Ex.AssocLeft
    , binary "-" Ex.AssocLeft
    ]
    ,
    [ binary "<" Ex.AssocLeft
    , binary ">" Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr =
  Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = do
  Var <$> identifier

function :: Parser Expr
function = do
  reserved "func"
  name <- identifier
  args <- many identifier
  colon
  body <- expr
  reserved "end"
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
  args <- parens $ commaSep expr
  return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond    <- expr
  colon
  tr       <- expr
  reserved "else"
  fl       <- expr
  reserved "end"

  return $ If cond tr fl

letvar :: Parser Expr
letvar = do
  reserved "using"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  colon
  body <- expr
  reserved "end"

  return $ foldr (uncurry Let) body defs

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  colon
  body <- expr
  reserved "end"

  return $ For var start cond step body

factor :: Parser Expr
factor =
      try floating
  <|> try int
  <|> try call
  <|> try variable
  <|> ifthen
  <|> try letvar
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
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s =
  parse (contents expr) "<you fucked up>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s =
  parse (contents toplevel) "<you fucked up>" s
