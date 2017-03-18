(** create a training set from a csv file *)
open Csv_io
open Printf
open Dog_t

module List = Utils.List


let index =
  let rec loop i map = function
    | [] -> map
    | h :: t ->
      let map = Utils.IntMap.add i h map in
      loop (i+1) map t
  in
  fun list ->
    loop 0 Utils.IntMap.empty list

(* column index * row index * value *)
type 'a cell = {
  cell_column: int;
  cell_row: int;
  cell_value: 'a;
}

type csv_cell = Csv_types.value cell

(* order by feature_id [j_*] in ascending order, then by observation
   in ascending order *)
let compare_cells c1 c2 =
  match Pervasives.compare c1.cell_column c2.cell_column with
    | 0 -> Pervasives.compare c1.cell_row c2.cell_row
    | x -> x

(* the type of a value is encoded in two bits *)
let tag_of_cell = function
  | { cell_value = `Int _ }    -> 0b01
  | { cell_value = `Float _ }  -> 0b10
  | { cell_value = `String _ } -> 0b11

let map_cell f cell = {
  cell_column = cell.cell_column;
  cell_row = cell.cell_row;
  cell_value = f cell.cell_value;
}

(* write the tags of (up to) four cells into the lower byte of an
   integer, with the tag of the first cell in bits 8 and 7 of that
   byte. *)
let tag4_of_cells offset (cells : csv_cell array) =
  let b = ref 0 in
  let num_cells_1 = (Array.length cells) - 1 in
  let offset_3 = offset + 3 in
  for i = offset to min offset_3 num_cells_1 do
    let tag = tag_of_cell cells.(i) in
    b := (!b lsl 2) lor tag
  done;
  for i = num_cells_1+1 to offset_3 do
    b := !b lsl 2
  done;
  !b

let write_cells_to_file work_dir file_num (cells : csv_cell array) =
  let path = Filename.concat work_dir (string_of_int file_num) in
  let ouch = open_out path in
  let bobuf = Bi_outbuf.create_channel_writer ouch in
  for i = 0 to (Array.length cells)-1 do
    (* every set of four consecutive cells, we write a byte that
       encodes the type of value in each of those cells *)
    if i mod 4 = 0 then (
      let tag4 = tag4_of_cells i cells in
      Bi_outbuf.add_char bobuf (Char.chr tag4)
    );

    let cell = cells.(i) in
    let j = cell.cell_column in
    let i = cell.cell_row in
    match cell.cell_value with
      | `Int value ->
        TS_b.write_int_cell bobuf (j, i, value)
      | `Float value ->
        TS_b.write_float_cell bobuf (j, i, value)
      | `String value ->
        TS_b.write_string_cell bobuf (j, i, value)
  done;
  Bi_outbuf.flush_channel_writer bobuf;
  close_out ouch

let cell_stream path =
  let inch = open_in path in
  let bibuf = Bi_inbuf.from_channel inch in
  let tag4 = ref 0 in

  let next i =
    let rem = i mod 4 in
    try
      if rem = 0 then (
        let c = Bi_inbuf.read_char bibuf in
        tag4 := Char.code c
      );
      let tag =
        match rem with
          | 3 ->  !tag4 land 0b00_00_00_11
          | 2 -> (!tag4 land 0b00_00_11_00) lsr 2
          | 1 -> (!tag4 land 0b00_11_00_00) lsr 4
          | 0 -> (!tag4 land 0b11_00_00_00) lsr 6
          | _ -> assert false
      in
      match tag with
        | 0b01 -> (* [`Int] *)
          let cell_column, cell_row, value =
            TS_b.read_int_cell bibuf in
          Some {cell_column; cell_row; cell_value = `Int value }
        | 0b10 -> (* [`Float] *)
          let cell_column, cell_row, value =
            TS_b.read_float_cell bibuf in
          Some { cell_column; cell_row; cell_value = `Float value }
        | 0b11 -> (* [`String] *)
          let cell_column, cell_row, value =
            TS_b.read_string_cell bibuf in
          Some { cell_column; cell_row; cell_value = `String value }
        | 0b00 ->
          (* the trailing, unused bits of a tag4, indicating that we
             are at the end of the input buffer *)
          close_in inch;
          None
        | _ -> assert false

    with Bi_inbuf.End_of_input ->
      close_in inch;
      None
  in
  Stream.from next

type create = {
  input_path_opt : string option;
  output_path : string;
  max_density : float;
  max_cells_in_mem : int;
  no_header : bool;
  work_dir : string;
  max_width : int;
  allow_variable_length_dense_rows : bool;
}


let dummy_cell = {
  cell_row = -1;
  cell_column = -1;
  cell_value = `Float nan;
}

(* i := row index
   j := column index
   c := cell index
   f := file index
*)

let write_cells_to_work_dir work_dir header next_row config =
  (* let num_features = List.length header in *)

  let header_length_opt = match header with
    | [] -> None
    | l -> Some (List.length l)
  in

  let append_cell ~cells ~mcore ~c ~f (cell : csv_cell) =
    if c < config.max_cells_in_mem then (
      cells.( c ) <- cell;
      cells, mcore, c + 1, f
    )
    else
      let success, mcore = Mcore.reap mcore in
      if not success then (
        Utils.epr "[ERROR] one sorting task failed\n%!";
        exit 1
      );
      let mcore = Mcore.spawn mcore (fun () ->
        Printf.printf "[STRT] sorting files=%d cells=%d ... %!" f c;
        Array.fast_sort compare_cells cells;
        Printf.printf "[DONE] sorting files=%d cells=%d\n%!" f c;
        write_cells_to_file work_dir f cells;
      ) ()
      in
      let cells = Array.make config.max_cells_in_mem dummy_cell in
      cells.(0) <- cell;
      cells, mcore, 1, f + 1
  in

  let rec loop ~cells ~mcore ~cell_row ~f ~c prev_dense_row_length_opt =
    if cell_row mod 1000 = 0 then
      Printf.printf "files=%d rows=%d\n%!" f cell_row;

    match next_row () with
      | `Ok `EOF ->
        if c > 0 then (
          (* write trailing cells, if there are any *)
          let trailing_cells = Array.make c dummy_cell in
          (* copy *)
          Array.blit cells 0 trailing_cells 0 c;
          Array.fast_sort compare_cells trailing_cells;
          write_cells_to_file work_dir f trailing_cells;
          cell_row, f+1
        )
        else
          cell_row, f

      | `Ok (`Dense dense) ->
        let cells, mcore, dense_row_length, c, f = List.fold_left (
            fun (cells, mcore, cell_column, c, f) cell_value ->
              let cells, mcore, c, f =
                let cell = { cell_column; cell_row; cell_value } in
                append_cell ~cells ~mcore ~c ~f cell
              in
              cells, mcore, cell_column + 1, c, f
          ) (cells, mcore, 0, c, f) dense in

        (* check that all dense rows have the same length *)
        let dense_row_length_opt =
          match prev_dense_row_length_opt with
            | None -> Some dense_row_length (* first dense row *)
            | Some prev_dense_row_length ->
              if not config.allow_variable_length_dense_rows &&
                 prev_dense_row_length <> dense_row_length then (
                Printf.printf "dense row %d has length %d, which is \
                               different than length %d of previous \
                               dense rows.\n%!"
                  (cell_row+1) dense_row_length prev_dense_row_length;
                exit 1
              );
              Some dense_row_length
        in
        loop ~cells ~mcore ~cell_row:(cell_row + 1) ~f ~c dense_row_length_opt

      | `Ok (`Sparse sparse) ->
        let cells, mcore, c, f = List.fold_left (
            fun (cells, mcore, c, f) (cell_column, cell_value) ->
              let cell = { cell_column; cell_row; cell_value } in
              append_cell ~cells ~mcore ~c ~f cell
          ) (cells, mcore, c, f) sparse in
        loop ~cells ~mcore ~cell_row:(cell_row+1) ~f ~c prev_dense_row_length_opt

      | `SyntaxError err ->
        print_endline (Csv_io.string_of_error_location err);
        exit 1

      | `UnterminatedString line ->
        Printf.printf "unterminated string on line %d\n%!" line;
        exit 1

      | `IntOverflow (line, offending_string) ->
        Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1

  in
  let cells = Array.make config.max_cells_in_mem dummy_cell in
  let mcore = Mcore.create_mprocess () in
  let result = loop ~cells ~mcore ~cell_row:0 ~f:0 ~c:0 header_length_opt in
  let success = Mcore.sync mcore in
  if not success then (
    Utils.epr "[ERROR] one sorting task failed\n%!";
    exit 1
  );
  result

let csv_to_cells work_dir config =
  let { input_path_opt; no_header } = config in
  let ch =
    match config.input_path_opt with
      | None ->
        print_endline "reading csv data from standard input";
        stdin
      | Some path ->
        open_in path
  in
  match Csv_io.of_channel ~no_header ch with
    | `Ok (header, next_row) ->
      let num_rows, num_cell_files =
        write_cells_to_work_dir work_dir header next_row
          config in
      close_in ch;
      header, num_rows, num_cell_files

    | `SyntaxError err ->
      print_endline (Csv_io.string_of_error_location err);
      exit 1

    | `UnterminatedString line ->
      Printf.printf "unterminated string on line %d\n%!" line;
      exit 1

    | `IntOverflow (line, offending_string) ->
      Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
        offending_string line;
      exit 1

module CellMerge = Stream_merge.Make(
    struct
      type t = csv_cell
      let leq h1 h2 =
        compare_cells h1 h2 < 1
    end
  )

type kind_count = {
  n_float : int ;
  n_int : int;
  n_string : int
}

let kind_count_of_histogram hist =
  Hashtbl.fold (
    fun value count kc ->
      match value with
        | `Float _ -> { kc with n_float = kc.n_float + count }
        | `Int _ -> { kc with n_int = kc.n_int + count }
        | `String _ -> { kc with n_string = kc.n_string + count }

  ) hist {n_float=0; n_int=0; n_string=0}

module StringSet = Set.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
  )

module StringOMap = Map.Make(
  struct
    type t = string option
    let compare = Pervasives.compare
  end
  )

let indexes_of_cat values cat_x =
  let _, indexes = List.fold_left (
      fun (i, indexes) (ii, value) ->
        assert (i = ii);
        match value with
          | `String cat ->
            let indexes =
              if cat = cat_x then
                i :: indexes
              else
                indexes
            in
            i+1, indexes
          | `Int _ | `Float _ -> assert false
    ) (0, []) values in
  indexes

let rec exclude_none accu = function
  | [] -> List.rev accu
  | (Some h) :: t ->  exclude_none (h :: accu) t
  | None :: t -> exclude_none accu t

let exclude_none list =
  exclude_none [] list

let categorical_feature j kc hist n i_values feature_id_to_name config =
  let n_anonymous = n - kc.n_string in
  assert (n_anonymous >= 0);
  let category_to_count, num_categories =
    Hashtbl.fold (
      fun value count (category_to_count, num_categories) ->
        assert (count > 0);
        let s_value =
          match value with
            | `String s -> s
            | `Float _
            | `Int _ -> assert false
        in
        let category_to_count = (s_value, count) :: category_to_count in
        category_to_count, num_categories + 1
    ) hist ([], 0) in

  if n_anonymous = 0 then (
    if num_categories = 1 then
      `Uniform
    else (
      (* sort so that categories with lower counts first *)
      let category_to_count = List.sort (
          fun (_, count1) (_, count2) ->
            Pervasives.compare count1 count2
        ) category_to_count in

      (* categories with higher counts are first *)
      let categories = List.rev_map fst category_to_count in

      let cat_to_cat_id = Hashtbl.create num_categories in
      List.iteri (
        fun cat_id cat ->
          Hashtbl.replace cat_to_cat_id cat cat_id
      ) categories;

      let i_cats = List.rev_map (
        fun (i, value) ->
          match value with
            | `Int _ | `Float _ -> assert false
            | `String cat ->
              i, Hashtbl.find cat_to_cat_id cat
      ) i_values (* i_values are in i-reverse order *) in

      let cat_runs = Rle.encode_sparse n i_cats (-1) in
      let c_vector = `RLE cat_runs in

      let cat = `Cat {
        c_feature_id = j;
        c_feature_name_opt = feature_id_to_name j;
        c_categories = categories;
        c_cardinality = num_categories;
        c_anonymous_category = None;
        c_vector
      }
      in `NonUniform cat
    )
  ) else (
    (* some categories are anonymous; assume that this signals
       sparsity *)
    let category_to_count = List.rev_map (
        fun (category, count) -> Some category, count
      ) category_to_count in

    (* add anonymous category count *)
    let category_to_count = (None, n_anonymous) :: category_to_count in

    (* sort so that categories with lower counts first *)
    let category_to_count = List.sort (
        fun (_, count1) (_, count2) ->
          Pervasives.compare count1 count2
      ) category_to_count in

    (* now categories are sorted in decreasing frequency *)
    let categories = List.rev_map fst category_to_count in

    let cat_to_cat_id = Hashtbl.create num_categories in
    List.iteri (
      fun cat_id cat ->
        Hashtbl.replace cat_to_cat_id cat cat_id
    ) categories;

    let anon_cat_id = Hashtbl.find cat_to_cat_id None in

    let i_cats = List.rev_map (
      fun (i, value) ->
        match value with
          | `Int _ | `Float _ -> assert false
          | `String cat ->
            let cat_id = Hashtbl.find cat_to_cat_id (Some cat) in
            i, cat_id
    ) i_values in (* [i_values] are in i-reversed order *)

    (* exclude the anonymous category from the list of features;
       otherwise preseving the order of the (Some _) payloads *)
    let categories = exclude_none categories in
    let c_cardinality = num_categories + 1 in
    let cat_runs = Rle.encode_sparse n i_cats anon_cat_id in

    let c_vector = `RLE cat_runs in

    let cat = `Cat {
      c_feature_id = j;
      c_feature_name_opt = feature_id_to_name j;
      c_categories = categories;
      c_cardinality;
      c_anonymous_category = Some anon_cat_id;
      c_vector
    }
    in `NonUniform cat
  )

module DEBUG = struct
  let rec rand_hist max_total_count max_value_incr max_count accu total_count
      value =
    if total_count >= max_total_count then
      total_count, List.rev accu
    else
      let v_incr = Random.int max_value_incr in
      let value = value + 1 + v_incr in
      let count = 1 + Random.int max_count in
      let total_count = count + total_count in
      let accu = (value, count) :: accu in
      rand_hist max_total_count max_value_incr max_count accu total_count value

  let rand_hist ~max_total_count ~max_value_incr ~min_value ~max_count =
    rand_hist max_total_count max_value_incr max_count [] 0 min_value
end

let incr hist ?(by=1) k =
  let count =
    try
      Hashtbl.find hist k
    with Not_found ->
      0
  in
  Hashtbl.replace hist k (count + by)

let float_zero = `Float 0.0
let int_zero   = `Int   0

(* unbox, and convert to list, so we can sort, and return the list and
   the total number of distinct values *)

let ordinal_feature
    zero
    of_value
    to_breakpoints
    ~j
    i_values
    breakpoints
    ~n
    feature_id_to_name
    =
  let bounds = breakpoints.bounds in
  let repr_elements = breakpoints.repr_elements in
  let o_cardinality = Array.length repr_elements in
  if o_cardinality <= 1 then `Uniform else
    (* Attention: We need to find the bin, not the bound. We have n bins but n-1 bounds. *)
    let find x = Binary_search.find_r bounds x 0 (o_cardinality - 1) in
    let i_rank = List.rev_map (
      fun (i, value) ->
        let rank = find (of_value value) in
        i, rank
    ) i_values in (* [i_values] are in i-reversed order *)
    let zero_rank = find (of_value zero) in
    let rank_runs = Rle.encode_sparse n i_rank zero_rank in
    let o_vector = `RLE rank_runs in
    let ord = `Ord {
      o_feature_id = j;
      o_feature_name_opt = feature_id_to_name j;
      o_cardinality;
      o_vector;
      o_breakpoints = to_breakpoints breakpoints;
    } in
    `NonUniform ord

let float_or_int_feature
    ~j
    kind_count
    hist_table
    ~n
    i_values
    feature_id_to_name
    ~max_width =

  let n_anonymous = n - kind_count.n_int - kind_count.n_float in
  assert (n_anonymous >= 0);

  if kind_count.n_float > 0 then (
    (* this is a float feature *)

    (* augment the histogram with zero's, assuming this is the anonymous
       value *)
    if n_anonymous > 0 then
      (* we don't want any values with value=0, count=0 in [hist_list] *)
      incr hist_table ~by:n_anonymous float_zero;

    (* if this is a float feature, merge the number of (`Int 0) and
       (`Float 0.0) *)
    (try
       let int_zero_count = Hashtbl.find hist_table int_zero in
       Hashtbl.remove hist_table int_zero;
       incr hist_table ~by:int_zero_count float_zero
     with Not_found ->
       ()
    );

    let hist_array = Huf_hist.make_hist_array Csv_types.float_of_value nan hist_table in

    if Array.length hist_array = 1 then
      `Uniform
    else
      let bins = Huf_hist.Float.huf hist_array max_width in
      let bounds = Huf_hist.Float.bounds bins in
      let repr_elements = Huf_hist.repr_elements bins in
      let freq = Huf_hist.freq bins in
      let breakpoints = { bounds; repr_elements; freq } in
      ordinal_feature float_zero Csv_types.float_of_value
        (fun b -> `Float b) ~j i_values breakpoints ~n feature_id_to_name
  )
  else (
    (* the value are all ints, but we might cast them to floats if the
       cardinality execeeds the cap *)

    (* augment the histogram with zero's, assuming this is the anonymous
       value *)
    if n_anonymous > 0 then
      (* we don't want any values with value=0, count=0 in [hist_list] *)
      incr hist_table ~by:n_anonymous int_zero;

    let hist_array = Huf_hist.make_hist_array Csv_types.int_of_value 0 hist_table in
    let cardinality = Array.length hist_array in

    let max_cardinality = 1 lsl max_width in
    if cardinality <= max_cardinality then
      (* low cardinality int's are kept as ints *)
      let hist_array = Huf_hist.make_hist_array Csv_types.int_of_value 0 hist_table in
      let bins = Huf_hist.Int.huf hist_array max_width in
      let bounds = Huf_hist.Int.bounds bins in
      let repr_elements = Huf_hist.repr_elements bins in
      let freq = Huf_hist.freq bins in
      let breakpoints = { bounds; repr_elements; freq } in

      ordinal_feature int_zero Csv_types.int_of_value
        (fun b -> `Int b) ~j i_values breakpoints ~n feature_id_to_name

    else
      (* high-cardinality int features are represented as float features *)
      (* cap the width through down-sampling *)
      let hist_array = Huf_hist.make_hist_array Csv_types.float_of_value nan hist_table in
      let bins = Huf_hist.Float.huf hist_array max_width in
      let bounds = Huf_hist.Float.bounds bins in
      let repr_elements = Huf_hist.repr_elements bins in
      let freq = Huf_hist.freq bins in
      let breakpoints = { bounds; repr_elements; freq } in
      ordinal_feature float_zero Csv_types.float_of_value
        (fun b -> `Float b) ~j i_values breakpoints ~n feature_id_to_name
  )


(* information about a feature that contains an illegal mixture of
   types (string type cannot be mixed with numbers) *)
type mixed_type_feature = {
  mt_feature_id : feature_id;
  mt_feature_name : string option;
  mt_string_values : string list;
  mt_float_values : float list;
  mt_int_values : int list;
}

exception MixedTypeFeature of mixed_type_feature

let mixed_type_feature_exn mt_feature_id mt_feature_name i_values =
  let mt_string_values, mt_float_values, mt_int_values = List.fold_left (
      fun (string_values, float_values, int_values) (i, value) ->
        match value with
          | `String string_value ->
            string_value :: string_values, float_values, int_values
          | `Float float_value ->
            string_values, float_value :: float_values, int_values
          | `Int int_value ->
            string_values, float_values, int_value :: int_values
    ) ([], [], []) i_values
  in
  let mt = {
    mt_feature_id;
    mt_feature_name;
    mt_string_values;
    mt_float_values;
    mt_int_values
  } in
  raise (MixedTypeFeature mt)

let write_feature j i_values n dog feature_id_to_name config =
  let name_for_log = match feature_id_to_name j with
    | Some x -> x
    | None -> "<nameless>"
  in
  let hist = Hashtbl.create (n / 100) in
  let kind_count = List.fold_left (
      fun kind_count (i, value) ->
        incr hist value;
        match value with
          | `Float _  -> { kind_count with n_float  = kind_count.n_float  + 1 }
          | `Int _    -> { kind_count with n_int    = kind_count.n_int    + 1 }
          | `String _ -> { kind_count with n_string = kind_count.n_string + 1 }

    ) {n_float=0; n_int=0; n_string=0} i_values in
  if kind_count.n_string > 0 then
    if kind_count.n_float = 0 && kind_count.n_int = 0 then
      let cf = categorical_feature j kind_count hist n i_values
          feature_id_to_name config in
      match cf with
        | `Uniform ->
          Printf.printf "%d cat: %s (uniform)\n%!" j name_for_log

        | `NonUniform cat ->
          Printf.printf "%d cat: %s\n%!" j name_for_log;
          Dog_io.WO.add_feature dog cat
    else
      let feature_name = feature_id_to_name j in
      mixed_type_feature_exn j feature_name i_values

  else if kind_count.n_float > 0 || kind_count.n_int > 0 then

    let float_or_int_feat =
      float_or_int_feature ~j kind_count hist ~n i_values
        feature_id_to_name ~max_width:config.max_width
    in
    match float_or_int_feat with
      | `Uniform ->
        Printf.printf "%d num: %s (uniform)\n%!" j name_for_log

      | `NonUniform feat -> (
          match feat with
            | `Ord ord ->
              (match ord.o_breakpoints with
                | `Float _ ->
                  Printf.printf "%d fp : %s\n%!" j name_for_log;
                | `Int _ ->
                  Printf.printf "%d int: %s\n%!" j name_for_log
              );
              Dog_io.WO.add_feature dog feat

            | `Cat _ -> assert false
        )

  else (
    Printf.printf "%d (%s): implicit uniform\n%!" j name_for_log
  )


let pr_hist j i_values =
  let hist = Hashtbl.create 10_000 in (* TODO *)
  let kind_count = List.fold_left (
    fun kind_count (i, value) ->
      incr hist value;
      match value with
        | `Float _ -> { kind_count with n_float = kind_count.n_float + 1 }
        | `Int _ -> { kind_count with n_int = kind_count.n_int + 1 }
        | `String _ -> { kind_count with n_string = kind_count.n_string + 1 }

  ) {n_float=0; n_int=0; n_string=0} i_values in
  printf "%d: f=%d i=%d s=%d\n" j kind_count.n_float kind_count.n_int kind_count.n_string

let read_cells_write_features work_dir ~num_cell_files ~num_rows header config =
  let feature_id_to_name =
    let idx = index header in
    fun feature_id ->
      Utils.IntMap.find_opt feature_id idx
  in

  let rec loop prev_column row_values dog =
    parser
  | [< '{cell_column; cell_row; cell_value}; tail >] ->
    if cell_column = prev_column then (
      let row_values = (cell_row, cell_value) :: row_values in
      loop cell_column row_values dog tail
    )
    else (
      write_feature prev_column row_values num_rows dog feature_id_to_name config;
      loop cell_column [(cell_row, cell_value)] dog tail
    )

  | [< >] ->
    write_feature prev_column row_values num_rows dog feature_id_to_name
      config;
    Dog_io.WO.close_writer dog
  in

  let dog = Dog_io.WO.create config.output_path num_rows in

  (* merge all the files to which cells were written in feature id,
     then (reverse) row id order *)
  let cell_streams = Utils.fold_range (
      fun file_num accu ->
        let cell_path = Filename.concat work_dir (string_of_int file_num) in
        (cell_stream cell_path) :: accu
    ) ~start:0 ~finix:num_cell_files [] in
  let merged_stream = CellMerge.create cell_streams in
  let {cell_column; cell_row; cell_value} = Stream.next merged_stream in
  let row_values = [cell_row, cell_value] in
  loop cell_column row_values dog merged_stream;

  (* remove all the cell files *)
  Utils.iter_range (
    fun file_num ->
      let cell_path = Filename.concat work_dir (string_of_int file_num) in
      Unix.unlink cell_path
  ) 0 num_cell_files

let create_work_dir () =
  let home = Unix.getenv "HOME" in
  let dot_dawg = Filename.concat home ".dawg" in
  Utils.mkdir_else_exit dot_dawg;
  let pid_s = string_of_int (Unix.getpid ()) in
  let work_dir = Filename.concat dot_dawg pid_s in
  Utils.mkdir_else_exit work_dir;
  work_dir

let pr = Printf.printf
let sp = Printf.sprintf

let create_dog config =
  let work_dir = create_work_dir () in
  let header, num_rows, num_cell_files = csv_to_cells work_dir config in
  pr "num rows: %d, num_cell_files: %d\n%!" num_rows num_cell_files;
  let exit_status =
    if num_rows > 0 then (
      try
        read_cells_write_features work_dir ~num_rows ~num_cell_files header
          config;
        0
      with (MixedTypeFeature mt) ->
        let feature_name =
          match mt.mt_feature_name with
            | Some fn -> sp ", name %S" fn
            | None -> ""
        in
        pr "feature %d (column %d%s) has feature values that are both numeric \
            and categorical\n" mt.mt_feature_id (mt.mt_feature_id + 1)
          feature_name;

        (* we should always have some string values in a mixed-type error *)
        pr "sample string  values: %s\n%!"
          (String.concat ", " (List.first 5 mt.mt_string_values));

        (match mt.mt_int_values with
          | [] -> ()
          | _ ->
            let int_values_s = List.map string_of_int
                (List.first 5 mt.mt_int_values) in
            pr "sample integer values: %s\n%!"
              (String.concat ", " int_values_s)
        );

        (match mt.mt_float_values with
          | [] -> ()
          | _ ->
            let float_values_s = List.map string_of_float
                (List.first 5 mt.mt_float_values) in
            pr "sample float   values: %s\n%!"
              (String.concat ", " float_values_s)
        );
        1
    )
    else (
      pr "input %scontains no data\n%!" (
        match config.input_path_opt with
          | Some path -> sp "%S " path
          | None -> ""
      );
      1
    )
  in

  (* remove the working directory *)
  (try
    Unix.rmdir work_dir
   with Unix.Unix_error _ ->
     (* directory has contents; we don't bother trying to cleanup
        well *)
     ()
  );
  exit_status


module Defaults = struct
  let max_width = 8
  let max_density = 0.1
  let max_cells_in_mem = 10_000_000
  let work_dir =
    let home = Unix.getenv "HOME" in
    let dot_dawg = Filename.concat home ".dawg" in
    dot_dawg
  let allow_variable_length_dense_rows = false
end

let csv_to_dog output_path input_path_opt max_density no_header max_cells_in_mem
    max_width =
  if max_density > 1.0 || max_density < 0.0 then (
    printf "max-density must be between 0 and 1 (inclusive)";
    exit 1
  );

  if max_cells_in_mem < 1 then (
    printf "max-cells-in-mem must be positive";
    exit 1
  );

  if max_width < 1 || max_width > 63 then (
    printf "max-width must be between 1 and 63 (inclusive)";
    exit 1
  );

  let config = {
    input_path_opt;
    output_path;
    max_density;
    no_header;
    max_cells_in_mem;
    work_dir = Defaults.work_dir;
    max_width;
    allow_variable_length_dense_rows =
      Defaults.allow_variable_length_dense_rows;
  } in
  let exit_status = create_dog config in

  (* hmm unnecessary verbage to make compiler happy *)
  let () = exit exit_status in
  ()

open Cmdliner

let commands =
  let create_cmd =
    let doc = "create dog files from csv inputs" in

    let output_path =
      let doc = "path of the output dog file" in
      Arg.(required & opt (some string) None &
           info ["o";"output"] ~docv:"PATH" ~doc)
    in

    let input_path =
      let doc = "path of input csv file; if absent, stdin is read instead" in
      Arg.(value & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in

    let max_density =
      let doc = "bitvectors should be considered dense when the minority bit \
                 represents less than this fraction of the length of the vector; \
                 this does not affect the output file, but may affect the speed \
                 at which it is generated" in
      Arg.(required & opt (some float) (Some Defaults.max_density) &
           info ["d";"bitvector-density"] ~docv:"FLOAT" ~doc)
    in

    let no_header =
      let doc = "interpret the first line of the csv file as data, rather
                 than a header providing names for the fields in file" in
      Arg.(value & flag & info ["h";"no-header"] ~doc)
    in

    let max_cells_in_mem =
      let doc = "the maximum number of csv data cells that will be held in memory; \
                 a smaller value will lead to more intermediate files, and overall \
                 slower processing: defaults to 10_000_000, requiring about 2GB of \
                 memory." in
      Arg.(required & opt (some int) (Some Defaults.max_cells_in_mem) &
           info ["m";"max-cells-in-mem"] ~docv:"INT" ~doc)
    in

    let max_width =
      let doc = "the maximum number of bitvectors that can represent a feature; \
                 if 2^max_width is smaller than the number of distinct values of \
                 a feature, the feature will be down-sampled" in
      Arg.(required & opt (some int) (Some Defaults.max_width) &
           info ["w";"max-width"] ~docv:"INT" ~doc)
    in

    Term.(
      pure csv_to_dog $
        output_path $
        input_path $
        max_density $
        no_header $
        max_cells_in_mem $
        max_width
    ),
    Term.info "csv" ~doc
  in

  [create_cmd]
