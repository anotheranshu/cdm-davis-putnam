open Core.Std
open Types
open State

(* The assignment stack will always be an Asgn_trace, and the cnf that it was trying to solve. *)

(* unravel_stack takes a cnf and assignment stack and reverts it back to the last time there
   was a choice of assignments to make, returning the cnf that was to be solved at that point
   as well as the stack with the bad choice already logged.  *)
let rec unravel_stack cnf l = 
  match l with 
  | [] -> raise (Cleanup.Unsolvable cnf)
  | (Asgn_trace.Forced _, next_cnf)::ls -> unravel_stack next_cnf ls
  | (Asgn_trace.Choice choices, next_cnf)::ls ->
      if (choices.Asgn_trace.true_chosen && choices.Asgn_trace.false_chosen)
      then unravel_stack next_cnf ls
      else (next_cnf, l) 

let single_lit_to_asgn lit =
  match lit with
  | Cnf.Literal.Pure s -> (s, true)
  | Cnf.Literal.Neg s -> (s, false)

let asgn_to_single_lit (s, asgn) = if asgn then (Cnf.Literal.Pure s) else (Cnf.Literal.Neg s)

(* Tries to split the current CNF on a new variable.  Does not check if it should continue, so only
   call this after checking that you should keep going. Does not clean up. *)
let split_new_variable state = 
  let next_literal = Heuristics.next_literal state.heuristic state.current_cnf in
  let (s, asgn) = single_lit_to_asgn next_literal in
  let this_choice =
    if asgn
    then Asgn_trace.({true_chosen = true; false_chosen = false; variable = s})
    else Asgn_trace.({true_chosen = false; false_chosen = true; variable = s})
  in
  try
    let (new_cnf, _) = 
      Cleanup.fixed_literals_to_new_cnf (Cnf.Literal.Set.singleton next_literal) state.current_cnf 
    in
    {
      stack = ((Asgn_trace.Choice this_choice, state.current_cnf)::(state.stack));
      current_cnf = new_cnf;
      continue = true;
      heuristic = state.heuristic
    }
  with
    Cleanup.Unsolvable _ ->
    {
      stack = ((Asgn_trace.Choice this_choice, state.current_cnf)::(state.stack));
      current_cnf = state.current_cnf;
      continue = false;
      heuristic = state.heuristic
    }

(* We guarantee that we will only call this when the top of the stack has a choice that we can redo. 
   Assuming that this is true, it will try the option not taken earlier. *)
let second_choice state =
  match state.stack with
  | [] -> failwith "Tried to second-guess the empty stack"
  | (Asgn_trace.Forced _, _)::remaining -> failwith "Tried to change a forced assignment"
  | (Asgn_trace.Choice choice, cnf)::remaining ->
      let asgn = not (choice.Asgn_trace.true_chosen) in
      let lit = asgn_to_single_lit (choice.Asgn_trace.variable, asgn) in
      let new_choice = Asgn_trace.({true_chosen = true; false_chosen = true; variable = choice.variable}) in
      try
        let (new_cnf, _) =
          Cleanup.fixed_literals_to_new_cnf (Cnf.Literal.Set.singleton lit) cnf
        in
        {
          stack = ((Asgn_trace.Choice new_choice, cnf)::remaining);
          current_cnf = new_cnf;
          continue = true;
          heuristic = state.heuristic;
        }
      with
        Cleanup.Unsolvable _ ->
        {
          stack = ((Asgn_trace.Choice new_choice, cnf)::remaining);
          current_cnf = cnf;
          continue = false;
          heuristic = state.heuristic;
        }

(* If continue is true, step will clean up the CNF and pick a new variable to split on.
   If continue is false, step will revert back to the last time we had a choice and try
 the other choice. *)
let step state =
  printf "%s\n" (to_string state);
  if state.continue then
    let (forced_assignment, cleaned_cnf) = Cleanup.cleanup_cnf state.current_cnf in
    match cleaned_cnf with
    | [] -> {state with stack = (Asgn_trace.Forced forced_assignment, state.current_cnf)::(state.stack);
                        current_cnf = []}
    | _ -> split_new_variable {state with stack = ((Asgn_trace.Forced forced_assignment, state.current_cnf)::(state.stack));
                                     current_cnf = cleaned_cnf}
  else
    let (Asgn_trace.Choice choice, old_cnf)::_ = state.stack in
    let (old_cnf, stack) = unravel_stack state.current_cnf state.stack in
    second_choice {current_cnf = old_cnf; continue = false; stack; heuristic = state.heuristic}

let init heuristic cnf = {heuristic; current_cnf = cnf; continue = true; stack = []}

let rec solve state =
  match state.current_cnf with
  | [] -> true
  | _ ->
    try solve (step state)
  with
    Cleanup.Unsolvable _ -> false