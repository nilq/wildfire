module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer =
  Tok.makeTokenParser style
    where
      ops =
        [ "+"
        , "-"
        , "*"
        , "/"
        , ","
        , ">"
        , "<"
        , "="
        ]

      names =
        [ "func"
        , "summon"
        , "for"
        , "if"
        , "then"
        , "else"
        , "end"
        , "using"
        ]

      style =
        emptyDef
          { Tok.commentLine     = "#"
          , Tok.reservedOpNames = ops
          , Tok.reservedNames   = names
          }

integer =
  Tok.integer lexer

float =
  Tok.float lexer

parens =
  Tok.parens lexer

semiSep =
  Tok.semiSep lexer

colon =
  Tok.colon lexer

brackets =
  Tok.brackets lexer

braces =
  Tok.braces lexer

commaSep =
  Tok.commaSep lexer

identifier =
  Tok.identifier lexer

reserved =
  Tok.reserved lexer

reservedOp =
  Tok.reservedOp lexer

whitespace =
  Tok.whiteSpace lexer
