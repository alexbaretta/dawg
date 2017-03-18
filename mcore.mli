type t

val create_mprocess : unit -> t
val spawn : t -> ('a -> unit) -> 'a -> t
val sync : t -> bool
val reap : t -> bool * t
