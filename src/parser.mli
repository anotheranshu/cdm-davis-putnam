open Types

type token =
  | EOF
  | IDENT of (string)
  | NEG
  | OR
  | AND
  | LPAREN
  | RPAREN

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.t
