(* unit test for Huf_hist module *)

external identity : 'a -> 'a = "%identity"

let _ =
  let inch = open_in Sys.argv.(1) in
  let max_width = int_of_string Sys.argv.(2) in

  (* collect the set of distinct values in a hash table; the count is
     the value of each entry in the hash table *)

  let hist_table = Hashtbl.create 10 in
  (try
     while true do
       let line = input_line inch in
       let value = float_of_string line in
       let count =
         try
           Hashtbl.find hist_table value
         with Not_found ->
           0
       in
       Hashtbl.replace hist_table value (count+1)
     done
   with End_of_file ->
     ()
  );
  close_in inch;

  (* convert to a list *)
  let hist_array = Huf_hist.make_hist_array identity nan hist_table in

  let open Huf_hist in
  let bins = Float.huf hist_array max_width in

  Array.iter (
    fun { left; repr; right; count } ->
      Printf.printf "l=%f m=%f r=%f c=%d\n%!" left repr right count
  ) bins
