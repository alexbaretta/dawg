type feature_monotonicity_map = Dog_t.monotonicity Utils.IntMap.t

val partition_observations :
  bool array ->
  Feat.afeature ->
  'c Proto_t.split -> bool array * bool array

type 'c m = {
  max_depth : int;
  feature_map : Feat_map.t;
  feature_monotonicity_map : feature_monotonicity_map;
  splitter : 'c Loss.splitter;
}

val make : 'c m -> int -> bool array -> 'c Model_t.l_tree option
val shrink : float -> float Model_t.l_tree -> float Model_t.l_tree
val extract_features : 'c Model_t.l_tree -> Utils.IntSet.t
val mk_eval : int -> 'c ->
  ( (Dog_t.feature_id -> Feat.afeature) -> 'c Model_t.l_tree -> 'c array )
