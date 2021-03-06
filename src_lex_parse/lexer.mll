{
  open Parser
  exception Eof
}
rule token = parse
    [' ' '\t' '\n']                   { token lexbuf }
  | ')'                               { RPAREN }
  | '('                               { LPAREN }
  | 'N'                               { NEG }
  | 'O'                               { OR }  
  | 'A'                               { AND }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident { IDENT ident }
  | eof                               { EOF }