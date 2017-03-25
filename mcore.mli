type t

val create_mprocess : int -> t
val spawn : t -> ('a -> unit) -> 'a -> unit
val sync : t -> bool
val reap : t -> bool
