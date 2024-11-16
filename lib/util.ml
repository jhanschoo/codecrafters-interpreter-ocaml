open Core

let number_to_string (n : float) : string =
  let s = Float.to_string n in
  if not (String.contains s '.') then s ^ ".0"
  else if Char.(String.nget s (-1) = '.') then s ^ "0"
  else s

let number_value_to_string (n : float) : string =
  let s = Float.to_string n in
  if Char.(String.nget s (-1) = '.') then
    String.slice s 0 (-1)
  else s

