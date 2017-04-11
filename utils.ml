let pr = Printf.printf
let epr = Printf.eprintf

let round x = match compare x 0.0 with
  | 1 -> truncate (x +. 0.5)
  | -1 -> truncate (x -. 0.5)
  | 0 -> 0
  | _ -> assert false

let iceil x = truncate (ceil x)
let ifloor x = truncate (floor x)

let sort_and_dedup l =
  let l = List.sort Pervasives.compare l in
  List.fold_left (fun accu x ->
    match accu with
      | [] -> [x]
      | hd :: tl when x = hd -> accu
      | _ -> x :: accu
  ) [] l

let add_to r x = r := !r +. x

let divide_up n d =
  ((n - 1) / d) + 1

let rand_bools density_true n =
  assert (density_true >= 0.0 && density_true <= 1.0);
  let rec loop i accu =
    if i = n then
      accu
    else
      let b = Random.float 1.0 < density_true in
      loop (i+1) (b :: accu)
  in
  loop 0 []

let string_of_bools bools =
  "[" ^ (String.concat ";" (List.map string_of_bool bools)) ^ "]"

let string_of_bools bools =
  let buf = Buffer.create 100 in
  let rec loop i = function
    | h :: t ->
      if i > 0 && i mod 7 = 0 then
        Buffer.add_string buf "\n ";
      let c =
        if h then
          '1'
        else
          '0'
      in
      Buffer.add_char buf c;
      loop (i+1) t
    | [] ->
      Buffer.add_string buf " ]\n";
  in
  Buffer.add_string buf "[\n ";
  loop 0 bools;
  Buffer.contents buf


let repeat n f =
  for i = 0 to n-1 do
    f ()
  done

let time f =
  let tick = Unix.gettimeofday () in
  let y = f () in
  let tock = Unix.gettimeofday () in
  y, tock -. tick


let f_xor b1 b2 =
  match b1, b2 with
    | false, false -> false
    | true , true  -> false
    | true , false -> true
    | false, true  -> true

let f_and_not b1 b2 =
  match b1, b2 with
    | false, false -> false
    | true , true  -> false
    | true , false -> true
    | false, true  -> false

let default d = function
  | Some x -> x
  | None -> d

module type XSetS = sig
  include Set.S
  val to_list : t -> elt list
  val of_list : elt list -> t
  val dedup_list : elt list -> elt list
end

module XSet ( M : Set.OrderedType) : XSetS with type elt = M.t = struct
  include Set.Make(M)
  let to_list set =
    fold (fun elt accu -> elt :: accu) set []

  let of_list l =
    List.fold_left (fun accu e -> add e accu) empty l

  let dedup_list l = to_list (of_list l)
end

module type XMapS = sig
  include Map.S
  val find_opt : key -> 'a t -> 'a option
  val find_assert : key -> 'a t -> 'a
end

module XMap ( M : Map.OrderedType ) = struct
  include Map.Make( M )

  let find_opt k t =
    try
      Some (find k t)
    with Not_found ->
      None

  let find_assert k t =
    try
      find k t
    with Not_found ->
      assert false
end


module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module Float = struct
  type t = float
  let compare = Pervasives.compare
end

module IntSet = XSet(Int)
module IntMap = XMap(Int)
module StringSet = XSet(String)
module StringMap = XMap(String)
module FloatSet = XSet(Float)

(* [log2 x] returns pair [y, s], where [y + 1] is the highest bit index
   whose of [x] value is 1; and [s], the sum of bits whose
   value is one, up to but excluding the highest bit index.  *)
let log2 =
  let rec loop x r one_count =
    if x = 0 then
      r - 1, one_count - 1
    else
      let z =
        if x land 1 = 1 then
          1
        else
          0
      in
      let one_count = one_count + z in
      let r = r + 1 in
      loop (x lsr 1) r one_count
  in
  fun x ->
    if x <= 0 then
      raise (Invalid_argument "log2")
    else
      loop x 0 0

let width x =
  let y, s = log2 x in
  let has_remainder = s > 0 in
  if has_remainder || y = 0 then
    (* ceil *)
    y + 1
  else
    y

let num_bytes card =
  divide_up (width card) 8


let rec fold_range f ~start ~finix x =
  if start < finix then
    let x = f start x in
    fold_range f ~start:(start+1) ~finix x
  else
    x

let rec iter_range f ~start ~finix =
  if start < finix then (
    f start;
    iter_range f ~start:(start+1) ~finix
  )

let mkdir_else_exit path =
  try
    Unix.mkdir path 0o750;
  with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | exn ->
      print_endline (Printexc.to_string exn);
      exit 1


(* read a biniou value from a file *)
let bi_read_from_file read path =
  let inch = open_in path in
  let binch = Bi_inbuf.from_channel inch in
  let v = read binch in
  close_in inch;
  v

(* (over)write  a biniou value from a file *)
let bi_write_to_file write path v =
  let ouch = open_out path in
  let bouch = Bi_outbuf.create_channel_writer ouch in
  write bouch v;
  Bi_outbuf.flush_channel_writer bouch;
  close_out ouch

(* returns a non-normalized absolute path *)
let abspath file_path =
  if Filename.is_relative file_path then
    Filename.concat (Unix.getcwd ()) file_path
  else
    file_path

(* Support OCaml 3.12 *)
module List = struct
  include List

  let iteri f l =
    let rec iteri i = function
      | hd :: tl -> f i hd; iteri (succ i) tl
      | [] -> ()
    in iteri 0 l

  let rec foldi_left_from f i init l =
    match l with
      | hd :: tl -> foldi_left_from f (succ i) (f i init hd) tl
      | [] -> init

  let foldi_left f init l = foldi_left_from f 0 init l

  let rec first accu n list =
    if n = 0 then
      List.rev accu
    else
      match list with
        | h :: t ->
          first (h :: accu) (n-1) t
        | [] ->
          List.rev accu

  let first n list =
    if n < 0 then
      raise (Invalid_argument "List.first")
    else
      first [] n list

  let rec dedup_sorted l = match l with
    | x1 :: (x2 :: _) as tl when x1 = x2 -> dedup_sorted tl
    | x1 :: tl -> x1 :: dedup_sorted tl
    | [] -> []

end


module Array = struct
  include Array

  let rec foldi_left_from f i init a =
    if i < Array.length a then
      let next = f i init a.(i) in
      foldi_left_from f (succ i) next a
    else
      init

  let foldi_left f init a =
    foldi_left_from f 0 init a

  let rec foldi_right_from f i a init =
    if i >= 0 then
      let next = f i a.(i) init in
      foldi_right_from f (pred i) a next
    else
      init

  let foldi_right f a init =
    foldi_right_from f (Array.length a - 1) a init

  let iteri2 f a b =
    if Array.length a <> Array.length b then
      Printf.ksprintf failwith "Array.iteri2: length(a)=%d length(b)=%d"
        (length a) (length b);
    iteri (fun i ai -> f i ai b.(i)) a

  let split a =
    let l = Array.map fst a in
    let r = Array.map snd a in
    l, r

  let float_cumsum_left a b =
    foldi_left (fun i accu elem ->
      let accu = accu +. elem in
      b.(i) <- accu;
      accu
    ) 0.0 a

  let float_cumsum_right a b =
    foldi_right (fun i elem accu ->
      let accu = accu +. elem in
      b.(i) <- accu;
      accu
    ) a 0.0

  let fill_all a x = fill a 0 (Array.length a) x

  let foldmap_left f init a =
    let l = length a in
    if l = 0 then
      [||]
    else
      let init = f init a.(0) in
      let b = make l init in
      let rec loop i accu =
        if i < l then
          let accu = f accu a.(i) in
          b.(i) <- accu;
          loop (succ i) accu
        else
          b
      in
      loop 1 init

  let foldmap_right f a init =
    let l = length a in
    if l = 0 then
      [||]
    else
      let init = f a.(l-1) init in
      let b = make l init in
      let rec loop i accu =
        if i >= 0 then
          let accu = f a.(i) accu in
          b.(i) <- accu;
          loop (pred i) accu
        else
          b
      in
      loop (l - 2) init

  let foldmapi_left f init a =
    let l = length a in
    if l = 0 then
      [||]
    else
      let init = f 0 init a.(0) in
      let b = make l init in
      let rec loop i accu =
        if i < l then
          let accu = f i accu a.(i) in
          b.(i) <- accu;
          loop (succ i) accu
        else
          b
      in
      loop 1 init

  let foldmapi_right f a init =
    let l = length a in
    if l = 0 then
      [||]
    else
      let start = l - 1 in
      let init = f start a.(start) init in
      let b = make l init in
      let rec loop i accu =
        if i >= 0 then
          let accu = f i a.(i) accu in
          b.(i) <- accu;
          loop (pred i) accu
        else
          b
      in
      loop (start - 1) init
end

module Hashtbl = struct
  include Hashtbl

  let find_default ht key default =
    try Hashtbl.find ht key with Not_found -> default
end

module Stats = struct
  type 'a histogram = {
    repr_elements : 'a array;
    hist_array : float array;
    sum_n : float;
  }

  let cmp_fst_ascending (v1,_) (v2,_) = Pervasives.compare v1 v2
  let sort_fst_ascending array = Array.fast_sort cmp_fst_ascending array

  let histogram na elems weights =
    let ht = Hashtbl.create 256 in
    Array.iteri2 (fun i e w ->
      let w0 = Hashtbl.find_default ht e 0.0 in
      let w1 = w0 +. w in
      Hashtbl.replace ht e w1
    ) elems weights;
    let cardinality = Hashtbl.length ht in
    let hist_array = Array.make cardinality (na, 0.0) in
    let _cardinality, sum_n =
      Hashtbl.fold (
        fun v count (i, accu_n) ->
          hist_array.(i) <- (v, count);
          succ i, accu_n +. count
      ) ht (0, 0.0)
    in
    assert(cardinality = _cardinality);
    sort_fst_ascending hist_array;
    let repr_elements, hist_array = Array.split hist_array in
    { repr_elements; hist_array; sum_n }

  let rank_statistic histogram r =
    let { hist_array; repr_elements; sum_n } = histogram in
    let rank_w = r *. sum_n in
    let rec find_rank_statistic k prev_cum_w =
      assert (prev_cum_w < rank_w);
      let delta_w = hist_array.(k) in
      let cum_w = prev_cum_w +. delta_w in
      let excess_w = cum_w -. rank_w in
      if excess_w >= 0.0 then
        let repr = repr_elements.(k) in
        if k = 0 then
          repr
        else
          let prev_repr = repr_elements.(k-1) in
          let result = prev_repr +. (repr -. prev_repr) *. (delta_w -. excess_w) /. delta_w in
          epr "[DEBUG] rank_statistic: rank_w=%f, k=%d, repr=%f, prev=%f, excess_w=%f -> %f\n%!"
            rank_w k repr prev_repr excess_w result;
          result
      else
        find_rank_statistic (succ k) cum_w
    in
    find_rank_statistic 0 r

  (* let rank_statistics histogram rs = *)
  (*   let { hist_array; repr_elements; sum_n } = histogram in *)
  (*   let rank_w = r *. sum_n in *)
  (*   let rec find_rank_statistic k prev_cum_w = *)
  (*     assert (prev_cum_w < rank_w); *)
  (*     let delta_w = hist_array.(k) in *)
  (*     let cum_w = prev_cum_w +. delta_w in *)
  (*     let excess_w = cum_w -. rank_w in *)
  (*     if excess_w >= 0.0 then *)
  (*       let repr = repr_elements.(k) in *)
  (*       if k = 0 then *)
  (*         repr *)
  (*       else *)
  (*         let prev_repr = repr_elements.(k-1) in *)
  (*         let result = prev_repr +. (repr -. prev_repr) *. (delta_w -. excess_w) /. delta_w in *)
  (*         epr "[DEBUG] rank_statistic: rank_w=%f, k=%d, repr=%f, prev=%f, excess_w=%f -> %f\n%!" *)
  (*           rank_w k repr prev_repr excess_w result; *)
  (*         (result, k, prev_cum_w) *)
  (*     else *)
  (*       find_rank_statistic (succ k) cum_w *)
  (*   in *)
  (*   List.fold_left (fun *)

  (*   let rec loop prev_k prev_cum_w rs = *)
  (*     match rs with *)
  (*     | [] -> [] *)
  (*     | hd :: tl -> *)
  (*       let (result, k, cum_w) = find_rank_statistic 0 0.0 *)

end
