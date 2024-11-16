type t = Parser.token * Lexing.position * Lexing.position

val tokenize : Sedlexing.lexbuf -> Unit.t -> t

val filter_unknown
  :  ?print:Bool.t
  -> (Sedlexing.lexbuf -> Unit.t -> t)
  -> Sedlexing.lexbuf
  -> bool ref * (Unit.t -> t)
