type 'a minimizer = int -> int -> (int -> 'a) -> int * 'a

let rec fibs n1 n2 len =
  let n = n1 + n2 - 1 in
  if n >= len then (n, n1) else
    fibs n n1 len

let fail x = Printf.ksprintf failwith x
let epr x = Printf.eprintf (x ^^ "\n%!")

let rec minimize_loop ~f ~l ~l' ~r' ~r ~f_l ~f_l' ~f_r' ~f_r ~n1 ~n2 =
  let len = r - l + 1 in
  (* assert( *)
  (*   if not (n1 >= n2) then fail "n1 >= n2 failed: n1=%d n2=%d" n1 n2; *)
  (*   if not (len <= n1) then fail "len <= n1 failed: len=%d n1=%d l=%d l'=%d r'=%d r=%d" len n1 l l' r' r; *)
  (*   if not (len > n2) then fail "len > n2 failed: len=%d n2=%d l=%d l'=%d r'=%d r=%d" len n2 l l' r' r; *)
  (*   if not (l <= l' && l' <= r' && r' <= r) then fail "l <= l' <= r' <= r failed: l=%d l'=%d r'=%d r=%d" l l' r' r; *)
  (*   true *)
  (* ); *)
  match len with
  | 1 -> l, f_l
  | 2 ->
    if f_l <= f_r then
      l, f_l
    else
      r, f_r
  | 3 ->
    let m = l' in
    let f_m = f_l' in
    if f_l <= f_m then
      if f_l <= f_r then l, f_l else r, f_r
    else
      if f_m <= f_r then m, f_m else r, f_r
  | _ ->
    let left () =
      (* epr "left: f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d n1=% 4d n2=% 4d len=% 4d" *)
      (*   l f_l l' f_l' r' f_r' r f_r n1 n2 len; *)
      let n1 = n2 and n2 = min n2 (n1 - n2 + 1) in
      let r = r' and f_r = f_r'
      and r' = l' and f_r' = f_l' in
      let l' = r - n2 + 1 in
      let f_l' = f l' in
      (* let len = r - l + 1 in *)
      (* epr "  --> f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d n1=% 4d n2=% 4d len=% 4d" *)
      (*   l f_l l' f_l' r' f_r' r f_r n1 n2 len; *)
      minimize_loop ~f ~l ~l' ~r' ~r ~f_l ~f_l' ~f_r' ~f_r ~n1 ~n2
    in
    let right () =
      (* epr "rght: f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d n1=% 4d n2=% 4d len=% 4d" *)
      (*   l f_l l' f_l' r' f_r' r f_r n1 n2 len; *)
      let n1 = n2 and n2 = min n2 (n1 - n2 + 1) in
      let l = l' and f_l = f_l'
      and l' = r' and f_l' = f_r' in
      let r' = l + n2 - 1 in
      let f_r' = f r' in
      (* let len = r - l + 1 in *)
      (* epr "  --> f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d f(% 4d)=% 4d n1=% 4d n2=% 4d len=% 4d" *)
      (*   l f_l l' f_l' r' f_r' r f_r n1 n2 len; *)
      minimize_loop ~f ~l ~l' ~r' ~r ~f_l ~f_l' ~f_r' ~f_r ~n1 ~n2
    in
    if f_l' < f_r' then
      left ()
    else if f_l' > f_r' then
      right ()
    else if f_l < f_r then
      left ()
    else
      right ()

let rec minimize l r f =
  let len = r - l + 1 in
  let n1, n2 = fibs 2 1 len in
  match len with
    | len when len <= 0 -> fail "fibsearch: interval length must be positive l=%d r=%d len=%d" l r len
    | 1 -> l, f l
    | 2 ->
      let f_l = f l in
      let f_r = f r in
      if f_l <= f_r then
        l, f_l
      else
        r, f_r
    | 3 ->
      let m = l + 1 in
      let f_l = f l in
      let f_r = f r in
      let f_m = f m in
      if f_l <= f_m then
        if f_l <= f_r then l, f_l else r, f_r
      else
        if f_m <= f_r then m, f_m else r, f_r
    | _ ->
      let l' = r - n2 + 1 in
      let r' = l + n2 - 1 in
      let f_l' = f l' in
      let f_r' = f r' in
      let f_l = f l in
      let f_r = f r in
      let left () =
        (* r' becomes r, l stays the same, need to recompute l' and r' *)
        let n1 = n2 and n2 = min n2 (n1 - n2 + 1) in
        let r = r' and f_r = f_r' in
        let r' = l + n2 - 1 in
        let f_r' = if r' = l' then f_l' else f r' in
        let l' = r - n2 + 1 in
        let f_l' = f l' in
        minimize_loop ~f ~l ~l' ~r' ~r ~f_l ~f_l' ~f_r' ~f_r ~n1 ~n2
      in
      let right () =
        (* l' becomes l, r stays the same, need to recompute l' and r' *)
        let n1 = n2 and n2 = min n2 (n1 - n2 + 1) in
        let l = l' and f_l = f_l' in
        let l' = r - n2 + 1 in
        let f_l' = if l' = r' then f_r' else f l' in
        let r' = l + n2 - 1 in
        let f_r' = f r' in
        minimize_loop ~f ~l ~l' ~r' ~r ~f_l ~f_l' ~f_r' ~f_r ~n1 ~n2
      in
      if f_l' < f_r' then
        left ()
      else if f_l' > f_r' then
        right ()
      else if f_l < f_r then
        left ()
      else
        right ()


let rec minimize_exhaustive l r f =
  let best_arg = ref l in
  let best_val = ref (f l) in
  for i = l + 1 to r do
    let f_i = f i in
    if f_i < !best_val then (
      best_val := f_i;
      best_arg := i
    )
  done;
  (!best_arg, !best_val)

(* let f n = 3 * n * n - 37 * n *)
(* let x_ = minimize ~f 0 34 *)
(* let x_ = minimize ~f 0 14000 *)
