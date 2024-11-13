open Core

let tokenize (file_contents : string) : Token.t list =
  let lineref = ref 0
  and posref = ref 0 in
  let f (acc : Token.t list) (c : char) : Token.t list =
    posref := !posref + 1;
    Token.(
      let line = !lineref
      and pos = !posref in
      match c with
      | '(' -> { tt = LeftParen ; lexeme = "("; line ; pos } :: acc
      | ')' -> { tt = RightParen ; lexeme = ")"; line ; pos } :: acc
      | _ -> acc) in
  let rev_res = Token.{ tt = Eof ; lexeme = "" ; line = !lineref ; pos = !posref + 1 } :: String.fold file_contents ~init:[] ~f in
  List.rev rev_res