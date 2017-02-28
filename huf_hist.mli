val make_hist_array :
  ('a -> 't) -> 't -> ('a, int) Hashtbl.t -> ('t * int) array

type 't bin = {
  left: 't;
  right: 't;
  repr: 't;
  count: int;
}


module type HUF = sig
  type t
  val huf : (t * int) array -> int -> t bin array
  val bounds : t bin array -> t array
end

module Int : HUF with type t = int
module Float : HUF with type t = float

val repr_elements : 'a bin array -> 'a array
val freq : 'a bin array -> int array
