type col_type = [ `Cat | `Num | `Ignored | `Untyped ]
type col_header = string * col_type
type header = col_header list

(* type csv_value = [ `Csv_int of string | `Csv_float of string | `Csv_string of string ] *)
type value = [ `Int of string | `Float of string | `String of string ]
type parsed_value =
  | Int of int
  | Float of float
  | String of string
  | Ignored
type parsed_row = (int * parsed_value) list

type dense = value option list
type sparse = (int * value) list
type row = [ `Dense of dense | `Sparse of sparse | `EOF ]
(* type opt_row = value option list *)

type invalid_value = {
  col_name : string;
  col_type : col_type;
  invalid_value : string;
  invalid_type : [ `Int | `Float | `String];
}

exception Invalid_value of invalid_value

let string_of_coltype = function
  | `Cat -> "CAT"
  | `Num -> "NUM"
  | `Untyped -> "UNTYPED"
  | `Ignored -> "IGNORED"

let invalid_value col_name col_type v =
  let payload = match v with
    | `String invalid_value -> { col_name; col_type; invalid_value; invalid_type = `String }
    | `Float invalid_value -> { col_name; col_type; invalid_value; invalid_type = `Float }
    | `Int invalid_value -> { col_name; col_type; invalid_value; invalid_type = `Int }
  in
  raise (Invalid_value payload)

let int_of_value : parsed_value -> int = function
  | Int i -> i
  | Float x -> Printf.ksprintf invalid_arg "int_of_value: Float %f" x
  | String s -> Printf.ksprintf invalid_arg "int_of_value: String %s" s
  | Ignored -> invalid_arg "int_of_value: Ignored"

let float_of_value : parsed_value -> float = function
  | Float x -> x
  | Int i -> float i
  | String s -> Printf.ksprintf invalid_arg "float_of_value: String %s" s
  | Ignored -> invalid_arg "float_of_value: Ignored"

let string_of_value : parsed_value -> string = function
  | String s -> s
  | Float x -> Printf.ksprintf invalid_arg "string_of_value: Float %f" x
  | Int i -> Printf.ksprintf invalid_arg "string_of_value: Int %d" i
  | Ignored -> invalid_arg "string_of_value: Ignored"

let parse_value (col_header:col_header) csv_value =
  match col_header with
    | (col_name, `Cat) -> (
      match csv_value with
        | `Int s
        | `Float s
        | `String s -> String s
      )
    | (col_name, `Num) -> (
      match csv_value with
        | `Int s -> Int (int_of_string s)
        | `Float s -> Float (float_of_string s)
        | `String s -> invalid_value col_name `Num csv_value
      )
    | (col_name, `Untyped) -> (
      match csv_value with
        | `Int s -> Int (int_of_string s)
        | `Float f -> Float (float_of_string f)
        | `String s -> String s
      )
    | _, `Ignored -> assert false

let parse_dense_row (header:header) opt_row =
  let rec loop i accu count (header_tl:header) opt_row =
    match header_tl, opt_row with
      | _ :: header_tl, None :: opt_row_tl
      | (_, `Ignored) :: header_tl, _ :: opt_row_tl ->
        loop (succ i) accu count header_tl opt_row_tl
      | col_header :: header_tl, (Some v) :: opt_row_tl ->
        let parsed = parse_value col_header v in
        let accu = (i,parsed) :: accu in
        let count = count + 1 in
        loop (succ i) accu count header_tl opt_row_tl
      | [], [] -> accu, count
      | _ :: _, [] -> failwith "parse_opt_row_sparse: too few values relative to columns"
      | [], _ :: _ -> failwith "parse_opt_row_sparse: too many values relative to columns"
  in
  loop 0 [] 0 header opt_row

let parse_sparse_row (header:header) =
  let header_table = Hashtbl.create 16 in
  List.iteri (fun i col_header -> Hashtbl.add header_table i col_header) header;
  fun sparse_row ->
    List.fold_left (
      fun (accu, count) (i, csv_value) ->
        let h = Hashtbl.find header_table i in
        if snd h = `Ignored then accu, count else
          ((i, parse_value h csv_value) :: accu, count+1)
    ) ([], 0) sparse_row

let parse_row (header:header) : row -> parsed_row * int =
  let parse_dense = parse_dense_row header in
  let parse_sparse = parse_sparse_row header in
  function
    | `Dense opt_row -> parse_dense opt_row
    | `Sparse sparse_row -> parse_sparse sparse_row
    | `EOF -> raise End_of_file
