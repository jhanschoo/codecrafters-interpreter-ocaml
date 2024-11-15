open Core

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 3 then (
    eprintf !"Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = argv.(1) in
  let filename = argv.(2) in

  if String.(command <> "tokenize") then (
    Printf.eprintf "%s" [%string "Unknown command: %{command}\n"];
    exit 1);

  let file_contents = In_channel.with_file ~f:In_channel.input_all filename in

  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  Printf.eprintf "Logs from your program will appear here!\n";

  if String.length file_contents > 0 then
    let tokens = Scanner.tokenize file_contents in
    let f (token : Token.t) : unit =
      Printf.printf "%s\n" (Token.to_string_expected token) in
    List.iter tokens ~f
  else
    (* Uncomment this block to pass the first stage *)
    print_endline "EOF  null"; (* Placeholder, remove this line when implementing the scanner *)
    ()
