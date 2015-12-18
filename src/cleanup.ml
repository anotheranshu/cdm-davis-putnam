open Core.Std
open Types

exception Unsolvable of Cnf.t

let reverse_term v =
  match v with
  | Cnf.Literal.Pure s -> Cnf.Literal.Neg s
  | Cnf.Literal.Neg s -> Cnf.Literal.Pure s 

(* fixed_literals_to_new_cnf takes a CNF and a set of single-literal constraints which do not conflict
   and returns the necessary assignment of variables to satisfy those constraints, as well as the new CNF
   after assuming that assignment.  It is used in both unit clause elimination and pure literal elimination.
   Raises unsolvable if a clause goes to zero variables. *)
let fixed_literals_to_new_cnf fixed_lits old_cnf =
  let assignment = String.Map.of_alist_exn
    (List.map (Set.elements fixed_lits) ~f:(function | Cnf.Literal.Neg s -> (s, false) | Cnf.Literal.Pure s -> (s, true)))
  in
  let clause_unsatisfied clause = (Set.length (Set.inter fixed_lits clause) = 0) in
  let remaining_clauses = List.filter ~f:clause_unsatisfied old_cnf in
  let rejected_variables = Cnf.Literal.Set.of_list (List.map ~f:reverse_term (Set.elements fixed_lits)) in
  let new_cnf = List.map ~f:(fun s -> Set.diff s rejected_variables) remaining_clauses in 
  if (List.exists new_cnf ~f:(fun s -> Set.length s = 0)) then
    raise (Unsolvable old_cnf)
  else (new_cnf, assignment)

let unit_clause_elimination cnf =
  let unit_clauses = List.filter cnf ~f:(fun set -> (Set.length set) = 1) in
  let unit_vars = List.fold_left unit_clauses ~init:(Cnf.Literal.Set.empty) ~f:(Set.union) in
  let is_unsolvable var = Set.mem unit_vars (reverse_term var) in
  if (Set.fold_right unit_vars ~init:false ~f:(fun var old -> old || (is_unsolvable var)))
  then raise (Unsolvable cnf)
  else fixed_literals_to_new_cnf unit_vars cnf 

let pure_literal_elimination cnf =
  let all_literals = List.fold_left cnf ~init:(Cnf.Literal.Set.empty) ~f:(Set.union) in
  let pure_literals = Set.filter all_literals ~f:(fun var -> not (Set.mem all_literals (reverse_term var))) in
  printf "The pure literals are %s\n" (List.to_string ~f:(Cnf.Literal.to_string) (Set.elements pure_literals));
  fixed_literals_to_new_cnf pure_literals cnf 

(* Repeatedly applies UCE and PLE until we reach a fixed point. Returns the new CNF and the assignment that got it there.*)
let rec cleanup_cnf cnf =
  let f ~key v = 
    match v with
    | `Both _ -> failwith "Bug in cleanup"
    | `Left x -> Some x
    | `Right x -> Some x
  in
  let (cnf1, assignment1) = unit_clause_elimination cnf in
  let (cnf2, assignment2) = pure_literal_elimination cnf1 in
  let assignment = 
    Map.merge assignment1 assignment2 ~f
  in
  if (Map.length assignment > 0)
  then 
    let (child_assignment, final_cnf) = cleanup_cnf cnf2 in 
    (Map.merge assignment child_assignment ~f, final_cnf)
  else (assignment, cnf)