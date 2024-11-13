open Core

type t =
  { tt : Token_type.t
  ; lexeme : string
  ; line : int
  ; (* pos denotes the absolute position, not the 0-indexed position *)
    pos : int
  }
[@@deriving fields, sexp]
