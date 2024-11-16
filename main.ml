open Core

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3
  then (
    eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);
  let command = argv.(1) in
  let filename = argv.(2) in
  match command with
  | "tokenize" -> Lib.Run.tokenize filename
  | "parse" -> Lib.Run.parse filename
  | _ ->
    eprintf "Unknown command: %s\n" command;
    exit 1
;;
