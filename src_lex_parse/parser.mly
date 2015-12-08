%{
  open Types
%}
%token EOF
%token <string> IDENT
%token NEG OR AND
%token LPAREN RPAREN
%left OR
%left AND
%nonassoc NEGATE
%start main
%type <Ast.t> main
%%
main:
    expr EOF { $1 }
;
expr:
    IDENT     { Ast.Ident $1 }
  | LPAREN expr RPAREN { $2 }
  | expr OR expr { Ast.Bin ($1, $3, Binop.Or) }
  | expr AND expr { Ast.Bin ($1, $3, Binop.And) }
  | NEG expr %prec NEGATE { Ast.Neg $2 }
;
