open Core

type t =
  { tt : Token_type.t
  ; lexeme : string
  ; line : int
  ; (* pos denotes the absolute position, not the 0-indexed position *)
    pos : int
  }
[@@deriving fields, sexp]

let to_string_expected (token : t) : string =
  let tts = Token_type.to_string token.tt in
  match String.split ~on:'.' tts with
  | [ tc ; s ] -> [%string "%{tc} %{token.lexeme} %{s}"]
  | [ tc ] -> [%string "%{tc} %{token.lexeme} null"]
  | _ -> failwith "Invalid token type"