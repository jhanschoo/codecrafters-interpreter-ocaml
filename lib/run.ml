open Core

let gobble = MenhirLib.Convert.Simplified.traditional2revised Parser.gobble
let expression = MenhirLib.Convert.Simplified.traditional2revised Parser.expression

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
      try expression stream with
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
      try expression stream with
      | Parser.Error -> exit 65
    in
    let result = Interpreter.evaluate_expr (Environment.create None) ast in
    Value.to_string result |> print_endline
  in
  In_channel.with_file filename ~f
