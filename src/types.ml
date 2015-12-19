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

module Ast = struct
  module T = struct
    type t = Bin of (t * t * Binop.t) | Neg of t | Ident of string with sexp
    let rec to_string = function
      | Bin (ast1, ast2, oper) -> "(" ^ (to_string ast1) ^ " " ^ (Binop.to_string oper) ^ " " ^ (to_string ast2) ^ ")"
      | Neg ast -> "(-" ^ (to_string ast) ^ ")"
      | Ident s -> s
  end
  include T
end

module Cnf = struct
  module Literal = struct
    module T = struct
      type t = Pure of string | Neg of string with sexp
      let to_string = function
        | Pure s -> s
        | Neg s -> "-" ^ s

      let compare x y =
        match (x, y) with
        | (Pure _, Neg _) -> -1
        | (Neg _, Pure _) -> 1
        | (Neg xs, Neg ys) -> compare xs ys
        | (Pure xs, Pure ys) -> compare xs ys
    end
    include T
    include Comparable.Make(T)
  end
  let to_string cnf =
    let list_cnf = List.map ~f:(Set.elements) cnf in
    let string_clauses = List.map ~f:(List.map ~f:(Literal.to_string)) list_cnf in
    let clauses_sepd = List.map ~f:(List.intersperse ~sep:(" | ")) string_clauses in
    let all_strings = List.map ~f:(List.fold ~init:"" ~f:(^)) clauses_sepd in
    "(" ^ (List.fold ~init:"" ~f:(^) (List.intersperse ~sep:(") & (") all_strings)) ^ ")"
  type t = Literal.Set.t list
end


module Asgn_trace = struct
  type choice = {true_chosen : bool; false_chosen : bool; variable : string; current : bool} with sexp
  type t = Forced of bool String.Map.t | Choice of choice with sexp

  let to_string = function
  | Forced mapping ->
      let elem_to_string (lit, b) = Printf.sprintf "(%s, %B)" lit b in
      Printf.sprintf "Forced: %s" (List.to_string ~f:elem_to_string (Map.to_alist mapping))
  | Choice {true_chosen; false_chosen; variable; current} ->
      Printf.sprintf "Choosing %s to be %B. True chosen?: %B, False chosen?: %B" variable current true_chosen false_chosen
end

(* current_cnf is the CNF we're trying to solve.  Continue is true if in the next step we should split on a new variable,
   and is false otherwise.  The stack always holds the cnf that was being solved and the decision that was made to try to
   solve it.  The heuristic never changes.  It's how we're trying to solve the problem.
 *)
module State = struct
  type heuristic = [`Most_unsatisfied | `Small_clauses]
  type t = {current_cnf : Cnf.t; continue : bool; stack : (Asgn_trace.t * Cnf.t) list; heuristic : heuristic}

  let stack_elem_to_string (asgn_elem, cnf) =
    Printf.sprintf "Used (%s) to solve CNF %s\n" (Asgn_trace.to_string asgn_elem) (Cnf.to_string cnf)

  let to_string s =
    let stack_string = List.to_string ~f:(stack_elem_to_string) s.stack in
    if s.continue then
    Printf.sprintf "We will continue splitting from here.  Here is the CNF and stack:\n %s\n %s\n --------\n" (Cnf.to_string s.current_cnf) stack_string
    else
    Printf.sprintf "We will not continue splitting from here.  Here is the CNF and stack:\n %s\n %s\n --------\n" (Cnf.to_string s.current_cnf) stack_string
end