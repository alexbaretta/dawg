type t

val create_mprocess : int -> t
val create_msynchronous : unit -> t
val spawn : t -> ('a -> unit) -> 'a -> unit
val sync : t -> bool
val reap : t -> bool
