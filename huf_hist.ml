let cmp_fst_ascending (v1,_) (v2,_) = Pervasives.compare v1 v2
let sort_fst_ascending array = Array.sort cmp_fst_ascending array

let make_hist_array of_value na hist_table : ('t * int) array =
  let cardinality = Hashtbl.length hist_table in
  let hist_array = Array.make cardinality (na, 0) in
  let _cardinality =
    Hashtbl.fold (
      fun value count i ->
        let v = of_value value in
        hist_array.(i) <- (v, count);
        succ i
    ) hist_table 0
  in
  assert(cardinality = _cardinality);
  sort_fst_ascending hist_array;
  hist_array

type 't bin = {
  left: 't;
  right: 't;
  repr: 't;
  count: int;
}

let rec binary_search ~query:v a low high =
  if low = high then
    low
  else
    let mid = (low + high) / 2 in
    if v <= a.(mid) then
      binary_search ~query:v a low mid
    else
      binary_search ~query:v a (mid+1) high

let cdf_of_hist hist_array =
  let cum_sum = ref 0 in
  Array.(init (length hist_array) (fun i -> let new_sum = !cum_sum + snd hist_array.(i) in cum_sum := new_sum; new_sum))

let eval_cdf (cdf : int array) i =
  if i < 0 then 0
  else
    let i = min i (Array.length cdf - 1) in
    cdf.(i)

let singleton hist_array i =
  let (v, count) = hist_array.(i) in
  (* Utils.epr "[DEBUG] singleton: i=%d count=%d\n%!" i count; *)
  { left = v; repr = v; right = v; count = count }

let rec singletons hist_array startp endp accu =
  if startp > endp then
    accu
  else
    let bin = singleton hist_array endp in
    singletons hist_array startp (pred endp) (bin :: accu)

let count_range cdf startp endp =
  eval_cdf cdf endp - eval_cdf cdf (startp - 1)


module type ARITHMETIC = sig
  type t
  val zero : t
  val add : t -> t -> t
  val int_mul : int -> t -> t
  val int_div : t -> int -> t
end

module type HUF = sig
  type t
  val huf : (t * int) array -> int -> t bin array
  val bounds : t bin array -> t array
end

module Make(Arith:ARITHMETIC) : HUF with type t = Arith.t = struct
  type t = Arith.t
  open Arith

  let rec sum_values ~accu hist_array startp endp =
    if startp > endp then accu else
      let (v, count) = hist_array.(startp) in
      let accu = add accu (int_mul count v) in
      sum_values ~accu hist_array (startp + 1) endp

  let range hist_array cdf startp endp =
    let left, left_c = hist_array.(startp) in
    let total = sum_values ~accu:zero hist_array startp endp in
    let count = count_range cdf startp endp in
    let repr = int_div total count in
    let right, right_c = hist_array.(endp) in
    { left; repr; right; count }

  let rec huf_one ~nbits hist_array cdf (startp:int) (endp:int) accu =
    (* Utils.epr "[DEBUG] huf_one: startp=%d endp=%d\n%!" startp endp; *)
    assert (startp <= endp);
    let bins = 1 lsl nbits in
    let levels = endp - startp + 1 in (* Number of distinct values to split into bins *)
    if levels <= bins then
      singletons hist_array startp endp accu
    else if startp = endp then
      singleton hist_array endp :: accu
    else if nbits = 0 then
      range hist_array cdf startp endp :: accu
    else
      let count = count_range cdf startp endp in
      let median_count = (eval_cdf cdf (startp-1)) + count lsr 1 in
      let median_p = binary_search ~query:median_count cdf startp endp in
      let nbits = nbits - 1 in
      let min_levels_in_split = 1 lsl nbits in
      let endp_left = min (max median_p (startp + min_levels_in_split - 1)) (endp - min_levels_in_split) in
      let startp_right = max (min (median_p + 1) (endp - min_levels_in_split + 1)) (startp + min_levels_in_split) in
      (* Utils.epr "[DEBUG] huf_one: startp=%d(%d) endp=%d(%d) min_levels=%d median_count=%d median_p=%d endp_left=%d startp_right=%d\n%!" *)
      (*   startp cdf.(startp) endp cdf.(endp) min_levels_in_split median_count median_p endp_left startp_right; *)

      let accu = huf_one ~nbits hist_array cdf startp_right endp accu in
      let accu = huf_one ~nbits hist_array cdf startp endp_left accu in
      accu

  let huf hist_array max_width =
    let cdf = cdf_of_hist hist_array in
    let t = huf_one ~nbits:max_width hist_array cdf 0 (Array.length hist_array - 1) [] in
    (* assert (let repr = List.map (fun bin -> bin.repr) t in repr = List.sort compare repr); *)
    Array.of_list t

  let bounds bins =
    let n_bins = Array.length bins in
    Array.init (n_bins - 1) (fun i ->
      let right_before = bins.(i).right in
      let left_after = bins.(i+1).left in
      int_div (add right_before left_after) 2
    )

end

let repr_elements bins = Array.map (fun bin -> bin.repr) bins
let freq bins = Array.map (fun bin -> bin.count) bins

module IntArith = struct
  type t = int
  let zero = 0
  let add = ( + )
  let int_mul = ( * )
  let int_div = ( / )
end

module FloatArith = struct
  type t = float
  let zero = 0.0
  let add = ( +. )
  let int_mul k x = float k *. x
  let int_div x k = x /. float k
end

module Int = Make(IntArith)
module Float = Make(FloatArith)
