open Core.Std
open Types

let rec combine_cnfs cnf1 cnf2 =
  match cnf1 with
  | [] -> []
  | (x::xs) ->
      List.append (List.map ~f:(Cnf.Literal.Set.union x) cnf2) (combine_cnfs xs cnf2)

let rec transform_to_cnf ast =
  match ast with
  | Ast.Ident s -> [Cnf.Literal.Set.singleton (Cnf.Literal.Pure s)]
  | Ast.Neg (Ast.Neg child_ast) -> transform_to_cnf child_ast
  | Ast.Neg (Ast.Bin (ast1, ast2, Binop.And)) -> 
      transform_to_cnf (Ast.Bin (Ast.Neg ast1, Ast.Neg ast2, Binop.Or))
  | Ast.Neg (Ast.Bin (ast1, ast2, Binop.Or)) ->
      transform_to_cnf (Ast.Bin (Ast.Neg ast1, Ast.Neg ast2, Binop.And))
  | Ast.Neg (Ast.Ident s) ->
      [Cnf.Literal.Set.singleton (Cnf.Literal.Neg s)]
  | Ast.Bin (ast1, ast2, Binop.And) -> List.append (transform_to_cnf ast1) (transform_to_cnf ast2)
  | Ast.Bin (ast1, ast2, Binop.Or) -> combine_cnfs (transform_to_cnf ast1) (transform_to_cnf ast2)