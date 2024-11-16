open Core

type t = Parser.token * Lexing.position * Lexing.position

let tokenize (lexbuf : Sedlexing.lexbuf) : Unit.t -> t =
  let rec gen lbuf =
    match%sedlex lbuf with
    (* one-char tokens *)
    | '(' -> Parser.LEFT_PAREN
    | ')' -> Parser.RIGHT_PAREN
    | '{' -> Parser.LEFT_BRACE
    | '}' -> Parser.RIGHT_BRACE
    | ',' -> Parser.COMMA
    | '.' -> Parser.DOT
    | '-' -> Parser.MINUS
    | '+' -> Parser.PLUS
    | ';' -> Parser.SEMICOLON
    | '*' -> Parser.STAR
    (* one-or-two char tokens *)
    | "!=" -> Parser.BANG_EQUAL
    | '!' -> Parser.BANG
    | "==" -> Parser.EQUAL_EQUAL
    | '=' -> Parser.EQUAL
    | ">=" -> Parser.GREATER_EQUAL
    | '>' -> Parser.GREATER
    | "<=" -> Parser.LESS_EQUAL
    | '<' -> Parser.LESS
    (* comment *)
    | "//", Star (Compl '\n'), Opt '\n' -> gen lbuf
    | '/' -> Parser.SLASH
    (* whitespace *)
    | Plus (' ' | '\t' | '\n' | '\r') -> gen lbuf
    (* string *)
    | '"', Star (Compl '"'), '"' ->
      let lexeme = Sedlexing.Utf8.lexeme lbuf in
      Parser.STRING (String.slice lexeme 1 (-1))
    | '"', Star (Compl '"') -> Parser.UNKNOWN "Unterminated string."
    (* number *)
    | Plus ('0' .. '9'), '.', Plus ('0' .. '9') ->
      let lexeme = Sedlexing.Utf8.lexeme lbuf in
      Parser.NUMBER (Float.of_string lexeme)
    | Plus ('0' .. '9') -> Parser.NUMBER (Float.of_string (Sedlexing.Utf8.lexeme lbuf))
    (* keywords *)
    (* identifier *)
    | ('a' .. 'z' | 'A' .. 'Z' | '_'), Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') ->
      Parser.IDENTIFIER (Sedlexing.Utf8.lexeme lbuf)
    (* unknown *)
    | any -> Parser.UNKNOWN ("Unexpected character: " ^ Sedlexing.Utf8.lexeme lbuf)
    | eof -> Parser.EOF
    | _ -> failwith ("Invalid tokenization" ^ Sedlexing.Utf8.lexeme lbuf)
  in
  Sedlexing.with_tokenizer gen lexbuf
;;

let token_constructor_value_strings (t : Parser.token) =
  match t with
  | LEFT_PAREN -> "LEFT_PAREN", "null"
  | RIGHT_PAREN -> "RIGHT_PAREN", "null"
  | LEFT_BRACE -> "LEFT_BRACE", "null"
  | RIGHT_BRACE -> "RIGHT_BRACE", "null"
  | COMMA -> "COMMA", "null"
  | DOT -> "DOT", "null"
  | MINUS -> "MINUS", "null"
  | PLUS -> "PLUS", "null"
  | SEMICOLON -> "SEMICOLON", "null"
  | SLASH -> "SLASH", "null"
  | STAR -> "STAR", "null"
  | BANG -> "BANG", "null"
  | BANG_EQUAL -> "BANG_EQUAL", "null"
  | EQUAL -> "EQUAL", "null"
  | EQUAL_EQUAL -> "EQUAL_EQUAL", "null"
  | GREATER -> "GREATER", "null"
  | GREATER_EQUAL -> "GREATER_EQUAL", "null"
  | LESS -> "LESS", "null"
  | LESS_EQUAL -> "LESS_EQUAL", "null"
  | IDENTIFIER s -> "IDENTIFIER", "null"
  | STRING s -> "STRING", s
  | NUMBER f -> "NUMBER", Util.number_to_string f
  | AND -> "AND", "null"
  | CLASS -> "CLASS", "null"
  | ELSE -> "ELSE", "null"
  | FALSE -> "FALSE", "null"
  | FUN -> "FUN", "null"
  | FOR -> "FOR", "null"
  | IF -> "IF", "null"
  | NIL -> "NIL", "null"
  | OR -> "OR", "null"
  | PRINT -> "PRINT", "null"
  | RETURN -> "RETURN", "null"
  | SUPER -> "SUPER", "null"
  | THIS -> "THIS", "null"
  | TRUE -> "TRUE", "null"
  | VAR -> "VAR", "null"
  | WHILE -> "WHILE", "null"
  | EOF -> "EOF", "null"
  | UNKNOWN s -> "UNKNOWN", s
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
    | Parser.UNKNOWN e ->
      Printf.eprintf "[line %i] Error: %s\n" cpos.pos_lnum e;
      hasUnknown := true;
      aux ()
    | _ ->
      if print
      then (
        let constructor, value = token_constructor_value_strings raw_token in
        Printf.printf "%s %s %s\n" constructor (Sedlexing.Utf8.lexeme lexbuf) value);
      token
  in
  hasUnknown, aux
;;
