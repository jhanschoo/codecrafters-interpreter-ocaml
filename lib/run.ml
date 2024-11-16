open Core

let gobble = MenhirLib.Convert.Simplified.traditional2revised Parser.gobble

let expression = MenhirLib.Convert.Simplified.traditional2revised Parser.expression

let tokenize filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let (has_error, stream) = Scanner.filter_unknown ~print:true Scanner.tokenize lexbuf in
    let _ = gobble stream in
    if !has_error
    then exit 65
  in
  In_channel.with_file filename ~f
;;

let parse filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let (has_error, stream) = Scanner.filter_unknown ~print:false Scanner.tokenize lexbuf in
    let ast = expression stream in
    print_endline (Ast.to_string ast);
    if !has_error
    then exit 65
  in
  In_channel.with_file filename ~f