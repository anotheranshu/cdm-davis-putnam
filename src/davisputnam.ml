open Core.Std
open Types

type step_cmd = 
  Advance of int 
| Display_stack 
| Display_variable of string 
| Display_cnf 
| Display_assignment 
| Change_heuristic of string
| Solve
| Exit
| Help 
| Unknown

let help_string =
  "\'advance\': Perform one evaluation step\n
   \'advance n\': Perform n evaluation steps\n
   \'display stack\': Shows the stack of assignments so far\n
   \'display variable s\': Shows the current value of variable s\n
   \'display cnf\': Shows the current state of the CNF\n
   \'display assignment\': Shows all of the current assignments (unordered)\n
   \'change heuristic s\': Changes the heuristic to heuristic s\n
   \'solve\': Solves the current formula to completion\n
   \'exit\': Terminates without solving the formula\n
   \'help\': Shows this help message\n
  "

let to_latex s = s

let get_assignment stack =
  let to_map elem =
    match elem with
    | (Asgn_trace.Forced m, _) -> m
    | (Asgn_trace.Choice c, _) -> String.Map.singleton c.Asgn_trace.variable c.Asgn_trace.current
  in
  List.fold ~init:(String.Map.empty) ~f:(String.Map.merge ~f:(fun ~key res -> match res with | `Left x -> Some x | `Right x -> Some x | `Both _ -> failwith "Bug in assignment.")) 
    (List.map ~f:to_map stack)

let get_command () =
  let s_opt = In_channel.input_line In_channel.stdin in
  match s_opt with
  | None -> Unknown 
  | Some s -> (
    match String.strip s with
    | "display stack" -> Display_stack
    | "display cnf" -> Display_cnf
    | "display assignment" -> Display_assignment
    | "solve" -> Solve
    | "exit" -> Exit
    | "advance" -> Advance 1
    | "help" -> Help
    | _ -> (
      match String.chop_prefix s ~prefix:"advance" with
      | Some n_str -> (
          try
            Advance (Int.of_string (String.strip n_str))
          with
          _ -> Unknown
        )
      | None -> (
        match String.chop_prefix s ~prefix:"display variable" with
        | Some v -> Display_variable (String.strip v)
        | None -> (
          match String.chop_prefix s ~prefix:"change heuristic" with
          | Some h -> Change_heuristic (String.strip h)
          | None -> Unknown
        )
      )
    )
  )

exception Bad_heuristic_string

let parse_heuristic_string s =
  match s with
  | "most_unsatisfied" -> `Most_unsatisfied
  | "small_clauses" -> `Small_clauses
  | _ -> raise Bad_heuristic_string

let rec step_wrapper steps_left state_ref =
  let state = !state_ref in
  match steps_left with
  | None -> ()
  | Some n -> (
    if (n > 0) then 
      match state.State.current_cnf with
      | [] -> ()
      | _ -> (
        state_ref := Step.step state;
        step_wrapper (Some (n - 1)) state_ref
      )
  ) 

let step_mode cnf latex heuristic =
  let state = ref (Step.init heuristic cnf) in 
  let rep () =
    let cmd = get_command () in
    match cmd with
    | Advance n -> step_wrapper (Some n) state
    | Display_stack ->
      let stack_string = List.to_string ~f:(State.stack_elem_to_string) (! state).State.stack in
        Printf.printf "%s\n%!" stack_string
    | Display_variable s ->
      let stack = (! state).State.stack in
      (
        match Map.find (get_assignment stack) s with
        | None -> Printf.printf "The variable %s is either not in the CNF or has not been assigned at this point.\n%!" s
        | Some b -> Printf.printf "The variable %s is currently set as %B\n%!" s b
      )
    | Display_cnf ->
        if latex then
          Printf.printf "%s\n%!" (to_latex (Cnf.to_string ((! state).State.current_cnf)))
        else
          Printf.printf "%s\n%!" (Cnf.to_string ((! state).State.current_cnf))
    | Display_assignment ->
      let assignment = get_assignment ((! state).State.stack) in 
      let str = (Map.fold assignment ~init:("{") ~f:(fun ~key ~data s -> Printf.sprintf "%s(%s, %b), " s key data)) ^ "}" in
      if latex then Printf.printf "%s\n" (to_latex str) else Printf.printf "%s\n%!" str
    | Change_heuristic s -> (
        try 
          let immutable_state = !state in
          state := {immutable_state with heuristic = (parse_heuristic_string s)}
        with
          Bad_heuristic_string -> Printf.printf "Unknown heuristic %s \n%!" s
        )
    | Solve -> step_wrapper None state
    | Exit -> exit 0
    | Unknown -> Printf.printf "Unknown or ill-formatted command.  For a list of commands, type \"help\"\n%!"
    | Help -> Printf.printf "%s\n%!" help_string
  in
  let rec repl () =
    let immutable_state = !state in
    try 
      match immutable_state.State.current_cnf with
      | [] -> Printf.printf "Satisfiable \n%!"
      | _ -> rep (); repl ()
    with
      Cleanup.Unsolvable _ -> Printf.printf "Unsatisfiable \n%!"
  in repl ()


let process_file quiet step_through cnf_only latex heuristic_string filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parser.main Lexer.token lexbuf in
  let cnf = Naivecnf.transform_to_cnf ast in
  let heuristic = parse_heuristic_string heuristic_string in
  (if quiet then () else printf "Initial CNF is %s \n%!" (Cnf.to_string cnf));
  if cnf_only then () else (
    if step_through then step_mode cnf latex heuristic else (
      if Step.solve (Step.init `Most_unsatisfied cnf)
      then printf "Satisfiable \n%!"
      else printf "Unsatisfiable \n%!"
    )
  )
;;

let spec =
  let open Command.Spec in
  empty
  +> flag "-q" no_arg ~doc:"Quiet mode.  Only prints whether input is satisfiable or not."
  +> flag "-s" no_arg ~doc:"Step-through mode.  Presents a GDB-like interface to step through solving."
  +> flag "--just-cnf" no_arg ~doc:"Just print out the CNF and then terminate."
  +> flag "--latex" no_arg ~doc:"All commands output latex."
  +> flag "-h" (optional_with_default "most_unsatisfied" string) ~doc:"Specify which heuristic to use.  Default: most unsatisfied clauses."
  +> anon ("filename" %: string)
;;


let command =
  Command.basic
    ~summary:"Runs the Davis-Putnam algorithm on the specified file."
    ~readme:(fun () -> "Runs the David-Putnam algorithm on the specified file.")
    spec
    process_file 
;;

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
