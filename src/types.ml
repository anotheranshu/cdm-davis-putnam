open Core.Std

module Binop = struct
module T = struct
  type t = And | Or with sexp
  let to_string = function
    | And -> "&"
    | Or -> "|"
  let of_string = function
   | "|" -> Or
   | "&" -> And
   | _ -> failwith "Unknown binop"
end
include T
end

module AST = struct
module T = struct
  type t = Bin of (t * t * Binop.t) | Neg of t | Ident of string with sexp
  let rec to_string = function
    | Bin (ast1, ast2, oper) -> (to_string ast1) ^ " " ^ (Binop.to_string oper) ^ " " ^ (to_string ast2)
    | Neg ast -> "-" ^ (to_string ast)
    | Ident s -> s
end
include t
end
