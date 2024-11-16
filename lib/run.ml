open Core

let tokenize filename =
  let f (chan : In_channel.t) =
    let lexbuf = Sedlexing.Utf8.from_channel chan in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.gobble in
    let (has_error, stream) = Scanner.filter_unknown ~print:true Scanner.tokenize lexbuf in
    let _ = parser stream in
    if !has_error
    then exit 65
  in
  In_channel.with_file filename ~f
;;

(* let errs = Parser.gobble parser stream in *)
(* let gen =
      let eof = ref false in
      let gen' = Scanner.tokenize file in
      let aux () =
        if !eof
        then None
        else (
          let token = gen' () in
          match token with
          | (Token.{ tt = Eof; _ }, _, _) -> eof := true; Some token
          | (Token.{ tt = Unknown _; _ }, _, _) -> eof := true; Some token
          | _ -> Some token)
      in
      aux
    in
    let f (st : Scanner.t) : unit =
      Printf.printf "%s\n" (Scanner.to_string st)
    in
    Gen.iter f gen; *)
(* if not List.(is_empty errs)
   then exit 65 *)
