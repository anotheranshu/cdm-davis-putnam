open Core.Std
open Types

(* Returns the literal whose satisfaction will satisfy the most clauses *)
let most_unsatisfied cnf =
  let update_map count_map lit_set =
    let update_elem literal counts =
      Map.change counts literal (function | None -> Some 1 | Some x -> Some (x + 1))
    in
    Set.fold_right lit_set ~init:count_map ~f:update_elem
  in
  let final_counts = List.fold_left cnf ~init:(Cnf.Literal.Map.empty) ~f:update_map in
  let update_max ~key ~data (old_max_elt, old_max) =
    if (data > old_max) then (key, data) else (old_max_elt, old_max)
  in
  let (max_elt, _) = Map.fold final_counts ~init:(Map.min_elt_exn final_counts) ~f:update_max in
  max_elt

(* Returns the literal whose satisfaction will satisfy the most clauses of minimum size *)
let small_clauses cnf =
  let maxsize = List.fold_left cnf ~init:Int.max_value ~f:(fun oldmax s -> Int.max oldmax (Set.length s)) in
  most_unsatisfied (List.filter cnf ~f:(fun s -> (Set.length s) = maxsize))

let next_literal heuristic =
  match heuristic with
  | `Most_unsatisfied -> most_unsatisfied
  | `Small_clauses -> small_clauses