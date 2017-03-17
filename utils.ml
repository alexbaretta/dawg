let pr = Printf.printf
let epr = Printf.eprintf

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
end

module XSet ( M : Set.OrderedType) : XSetS with type elt = M.t = struct
  include Set.Make(M)
  let to_list set =
    fold (fun elt accu -> elt :: accu) set []
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

module IntSet = XSet(Int)
module IntMap = XMap(Int)

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

end
