open Core

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3
  then (
    eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);
  let command = argv.(1) in
  let filename = argv.(2) in
  if String.(command <> "tokenize")
  then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);
  Lib.Run.tokenize filename
;;
