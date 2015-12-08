open Core.Std

let process_file filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parser.main Lexer.token lexbuf in
  printf "success?\n\!"

  


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
