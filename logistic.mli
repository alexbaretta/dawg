(* Support binarization of ordinal features onto two labels *)
type binarization_threshold = [
  | `LTE of float (* positive label is LTE, negative case is GT *)
  | `GTE of float (* positive label is GTE, negative case is LT *)
  | `LT of float (* positive label is LT, negative case is GTE *)
  | `GT of float (* positive label is GT, negative case is LTE *)
]

val probability : float -> float
class splitter :
  max_gamma_opt:float option ->
    binarization_threshold_opt:binarization_threshold option ->
    weights:float array ->
    y_feature:Feat.afeature ->
    n_rows:int ->
    num_observations:int ->
    min_observations_per_node:float ->
      Loss.splitter
