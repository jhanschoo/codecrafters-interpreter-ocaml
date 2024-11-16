type t

val create : t option -> t
val get : t -> string -> Value.t option
val define : t -> string -> Value.t -> unit
val set : t -> string -> Value.t -> unit
