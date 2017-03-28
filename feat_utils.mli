val apply_max_gamma_opt : max_gamma_opt:float option -> float -> float -> float * float
val repr_of_afeature : Feat.afeature ->
  [ `Float of (int * float) array
  | `Int of (int * int) array
  | `String of (int * string) array
  | `StringAnon of (int * string option) array ]

open Dog_t
val id_of_feature : ('a, 'b) feature -> feature_id
val name_of_feature : ('a, 'b) feature -> feature_name option
val cardinality_of_feature : ('a, 'b) feature -> int
val vector_of_feature : ('a, 'b) feature -> ('a, 'b) Dog_t.vector

val folds_of_feature : n:int -> num_folds:int -> Feat.afeature ->
  [ `Folds of int array
  | `TooManyCategoricalFolds of int
  | `TooManyOrdinalFolds of int
  ]
(* [folds_of_feature ~n:num_observations ~num_folds feature] returns
   an array of fold memberships, assigning each observation to a fold.
   For categorical features, [num_folds] must match the cardinality of
   the feature exactly. *)

val weights_of_afeature : Feat.afeature -> float array

val i_to_a : (int -> Vec.t) -> Feat.ifeature -> Feat.afeature

type feature_descr = [ `Name of string | `Id of int ]

val string_of_feature_descr : feature_descr -> string
val feature_descr_of_string : string -> feature_descr option
val descr_of_feature : ('a, 'b) feature -> feature_descr
val string_descr_of_feature : ('a, 'b) feature -> string
val descr_of_cat_feature : ('a, 'b) cat_feature -> feature_descr
val descr_of_ord_feature : ('a, 'b) ord_feature -> feature_descr

val iter_ord_feature : (int -> float -> unit) -> (Vec.t, Vec.t) Dog_t.ord_feature -> unit
val iter_ord_by_level : (int -> int -> unit) -> (Vec.t, Vec.t) Dog_t.ord_feature -> unit
val repr_array_of_ord_feature : int -> (Vec.t, Vec.t) Dog_t.ord_feature -> float array

(* ys is a column-major matrix: we want every row to be contiguous in memory *)
val repr_table_of_ord_features:
  scale:float -> int -> (Vec.t, Vec.t) Dog_t.ord_feature list -> float array array

val repr_elements_of_ord_feature: ('a, 'b) Dog_t.ord_feature -> float array
