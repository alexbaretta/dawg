type col_type = [ `Cat | `Num | `Untyped ]
type col_header = string * col_type
type header = col_header list

type value = [ `Int of int | `Float of float | `String of string ]
type dense = value list
type sparse = (int * value) list
type row = [ `Dense of dense | `Sparse of sparse | `EOF ]
type opt_row = value option list

let int_of_value : value -> int = function
  | `Int x -> x
  | _ -> invalid_arg "int_of_value"

let float_of_value : value -> float = function
  | `Float x -> x
  | `Int x -> float x
  | _ -> invalid_arg "float_of_value"

let string_of_value : value -> string = function
  | `String x -> x
  | `Float f -> string_of_float f
  | `Int i -> string_of_int i

let parse_opt_row_sparse opt_row =
  let rec loop i accu opt_row =
    match opt_row with
    | None :: tl -> loop (succ i) accu tl
    | (Some v) :: tl -> loop (succ i) ((i,v) :: accu) tl
    | [] -> accu
  in
  loop 0 [] opt_row

exception Sparse
let unopt = function
  | Some x -> x
  | None -> raise Sparse

let parse_opt_row_dense opt_row = List.map unopt opt_row

let parse_opt_row opt_row =
  try `Dense(parse_opt_row_dense opt_row)
  with Sparse -> `Sparse(parse_opt_row_sparse opt_row)
