open Core

type t = Parser.token * Lexing.position * Lexing.position

let tokenize (lexbuf : Sedlexing.lexbuf) : Unit.t -> t =
  (* let lexbuf = Sedlexing.Latin1.from_channel chan in *)
  let gen lbuf =
    match%sedlex lbuf with
    | "!=" -> Parser.BANG_EQUAL
    | '!' -> Parser.BANG
    | "==" -> Parser.EQUAL_EQUAL
    | '=' -> Parser.EQUAL
    | '(' -> Parser.LEFT_PAREN
    | ')' -> Parser.RIGHT_PAREN
    | '{' -> Parser.LEFT_BRACE
    | '}' -> Parser.RIGHT_BRACE
    | ',' -> Parser.COMMA
    | ';' -> Parser.SEMICOLON
    | '.' -> Parser.DOT
    | '+' -> Parser.PLUS
    | '-' -> Parser.MINUS
    | '*' -> Parser.STAR
    | '/' -> Parser.SLASH
    | any -> Parser.UNKNOWN (Char.of_string (Sedlexing.Latin1.lexeme lbuf))
    | eof -> Parser.EOF
    | _ -> failwith ("Invalid token" ^ Sedlexing.Latin1.lexeme lbuf)
  in
  Sedlexing.with_tokenizer gen lexbuf
;;

let token_constructor_value_strings (t : Parser.token) =
  match t with
  | BANG_EQUAL -> "BANG_EQUAL", "null"
  | BANG -> "BANG", "null"
  | RIGHT_PAREN -> "RIGHT_PAREN", "null"
  | LEFT_PAREN -> "LEFT_PAREN", "null"
  | RIGHT_BRACE -> "RIGHT_BRACE", "null"
  | LEFT_BRACE -> "LEFT_BRACE", "null"
  | COMMA -> "COMMA", "null"
  | SEMICOLON -> "SEMICOLON", "null"
  | DOT -> "DOT", "null"
  | PLUS -> "PLUS", "null"
  | MINUS -> "MINUS", "null"
  | STAR -> "STAR", "null"
  | SLASH -> "SLASH", "null"
  | IDENTIFIER s -> "IDENTIFIER", s
  | EOF -> "EOF", "null"
  | EQUAL_EQUAL -> "EQUAL_EQUAL", "null"
  | EQUAL -> "EQUAL", "null"
  | UNKNOWN c -> "UNKNOWN", String.of_char c
;;

let filter_unknown
  ?(print : Bool.t = false)
  (tokenizer : Sedlexing.lexbuf -> Unit.t -> t)
  (lexbuf : Sedlexing.lexbuf)
  : bool ref * (Unit.t -> t)
  =
  let hasUnknown = ref false in
  let rec aux () : t =
    let stream = tokenizer lexbuf in
    let token = stream () in
    let raw_token, _, cpos = token in
    match raw_token with
    | Parser.UNKNOWN c ->
      Printf.eprintf "[line %i] Error: Unexpected character: %c\n" cpos.pos_lnum c;
      hasUnknown := true;
      aux ()
    | _ ->
      if print
      then (
        let constructor, value = token_constructor_value_strings raw_token in
        Printf.printf "%s %s %s\n" constructor (Sedlexing.Latin1.lexeme lexbuf) value);
      token
  in
  hasUnknown, aux
;;

(* let to_string ((token, start_p, curr_p) : t) : string =
   let tts = Token_type.to_string token.tt in
   let token = match String.split ~on:'.' tts with
   | [ tc ; s ] -> Printf.sprintf "%s %s %s" tc token.lexeme s
   | [ tc ] -> Printf.sprintf "%s %s null" tc token.lexeme
   | _ -> failwith "Invalid token type"
   in
   Printf.sprintf "%s [%i:%i-%i:%i]" token
   start_p.pos_lnum start_p.pos_cnum
   curr_p.pos_lnum curr_p.pos_cnum *)

(* let tokenize (file_contents : string) : result =
  let lineref = ref 1
  and posref = ref 0 in
  let f ((acc, errs) : result) (c : char) : result =
    posref := !posref + 1;
    let line = !lineref
    and pos = !posref in
    let single_char_tmpl (tt : Token_type.t) : result =
      { tt; lexeme = String.of_char c; line; pos } :: acc, errs
    in
    match c with
    | '(' -> single_char_tmpl Left_Paren
    | ')' -> single_char_tmpl Right_Paren
    | '{' -> single_char_tmpl Left_Brace
    | '}' -> single_char_tmpl Right_Brace
    | ',' -> single_char_tmpl Comma
    | '.' -> single_char_tmpl Dot
    | '-' -> single_char_tmpl Minus
    | '+' -> single_char_tmpl Plus
    | ';' -> single_char_tmpl Semicolon
    | '*' -> single_char_tmpl Star
    | '/' -> single_char_tmpl Slash
    | _ ->
      let e = Printf.sprintf "[line %i] Error: Unexpected character: %c" line c in
      Printf.eprintf "%s\n" e;
      acc, e :: errs
  in
  let acc, errs = String.fold file_contents ~init:([], []) ~f in
  ( Token.{ tt = Eof; lexeme = ""; line = !lineref; pos = !posref + 1 } :: acc |> List.rev
  , errs )
;; *)
