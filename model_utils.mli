val folds_l_to_c : Feat_map.t -> Model_t.l_folds ->
  Model_t.c_folds * Model_t.feature list

val trees_rle_to_array : ('a, Model_t.direction_rle) Model_t.trees ->
  ('a, Model_t.category_direction array) Model_t.trees
(* tranform trees, decoding the run-length-encoded representation of
   the category directions, if any *)

val folds_rle_to_array :
  ('a, Model_t.direction_rle) Model_t.folds ->
  ('a, Model_t.category_direction array) Model_t.folds

val rle_to_array :
  ('a, Model_t.direction_rle) Model_t.model ->
  ('a, Model_t.category_direction array) Model_t.model

val category_array_of_rle : Model_t.direction_rle ->
  Model_t.category_direction array

val tree_extract_stats : Proto_t.point list ->
  Proto_t.point -> ('a, 'b) Model_t.tree ->
  Proto_t.point list
