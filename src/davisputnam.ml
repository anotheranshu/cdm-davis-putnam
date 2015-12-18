open Core.Std

let process_file filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parser.main Lexer.token lexbuf in
  printf "AST is %s \n" (Types.Ast.to_string ast);
  let cnf = Naivecnf.transform_to_cnf ast in
  printf "Initial CNF is %s \n" (Types.Cnf.to_string cnf);
  if Step.solve (Step.init `Most_unsatisfied cnf)
  then printf "Satisfiable \n%!"
  else printf "Unsatisfiable \n%!"
;;

let spec =
  let open Command.Spec in
  empty
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
