open Core

let gobble = MenhirLib.Convert.Simplified.traditional2revised Parser.gobble
let expr = MenhirLib.Convert.Simplified.traditional2revised Parser.expr
let prog = MenhirLib.Convert.Simplified.traditional2revised Parser.prog

let tokenize filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let has_error, stream = Scanner.filter_unknown ~print:true Scanner.tokenize lexbuf in
    let _ = gobble stream in
    if !has_error then exit 65
  in
  In_channel.with_file filename ~f
;;

let parse filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let has_error, stream = Scanner.filter_unknown ~print:false Scanner.tokenize lexbuf in
    if !has_error then exit 65;
    let ast =
      try expr stream with
      | Parser.Error -> exit 65
    in
    print_endline (Ast.to_string ast)
  in
  In_channel.with_file filename ~f
;;

let evaluate filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let has_error, stream = Scanner.filter_unknown ~print:false Scanner.tokenize lexbuf in
    if !has_error then exit 65;
    let ast =
      try expr stream with
      | Parser.Error -> exit 65
    in
    let result = Interpreter.evaluate ast in
    Value.to_string result |> print_endline
  in
  In_channel.with_file filename ~f
;;

let run filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let has_error, stream = Scanner.filter_unknown ~print:false Scanner.tokenize lexbuf in
    if !has_error then exit 65;
    let program =
      try prog stream with
      | Parser.Error -> exit 65
    in
    Interpreter.execute program
  in
  In_channel.with_file filename ~f
;;
