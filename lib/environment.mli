type 'a t

val initialize : 'a -> 'a t
val create : 'a t -> 'a t
val get : 'a t -> string -> 'a option
val define : 'a t -> string -> 'a -> unit
val set : 'a t -> string -> 'a -> unit
