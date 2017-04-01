open Model_t
open Dog_t

module Array = Utils.Array
module IntMap = Utils.IntMap

(* translate from the feature type defined in [Dog_t] to [Model_t] *)
let feature_d_to_m = function
  | `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_categories;
      c_anonymous_category } ->

    `CategoricalFeature {
      cf_feature_id = c_feature_id;
      cf_feature_name_opt = c_feature_name_opt;
      cf_categories = c_categories;
      cf_anonymous_category_index_opt = c_anonymous_category;
    }

  | `Ord { o_feature_id; o_feature_name_opt } ->

    `OrdinalFeature {
      of_feature_id = o_feature_id;
      of_feature_name_opt = o_feature_name_opt
    }

(* convert int's to float's (model doesn't make a distinction, unlike
   the dog file, in which this has implications on compression).  We
   also convert to arrays, so we can easily get the breakpoint value
   corresponding to a split (index). *)
let float_array_of_breakpoints = function
  | `Int breakpoints -> Array.map float breakpoints.bounds
  | `Float breakpoints -> breakpoints.bounds

(* create a map from the feature id of ordinal features to their
   breakpoints *)
let id_to_breakpoints id_to_feature =
  Utils.IntMap.fold (
    fun feature_id feature map ->
      match feature with
        | `Ord { o_breakpoints } ->
          let float_array = float_array_of_breakpoints o_breakpoints in
          Utils.IntMap.add feature_id float_array map
        | `Cat _ -> map
  ) id_to_feature Utils.IntMap.empty

let rle_of_category_array directions =
  let _, _, rle = Rle.encode_dense (Array.to_list directions) in
  let dr_first_direction =
    match rle with
      | (_, `Left ) :: _ -> `Left
      | (_, `Right) :: _ -> `Right
      | _ -> assert false (* must have at least two direction! *)
  in
  let dr_run_lengths = List.rev_map fst (List.rev rle) in
  { dr_first_direction; dr_run_lengths }

let opposite_direction = function
  | `Right -> `Left
  | `Left  -> `Right

let category_array_of_rle { dr_first_direction; dr_run_lengths } =
  (* first, add a direction to each run length, so we can use [Rle.decode_runs_rev] *)
  let _, runs_rev = List.fold_left (
      fun (direction, runs_rev) run_length ->
        let runs_rev = (run_length, direction) :: runs_rev in
        let direction = opposite_direction direction in
        direction, runs_rev
    ) (dr_first_direction, []) dr_run_lengths in
  let _, directions = Rle.decode_rev runs_rev in
  Array.of_list directions

let rec tree_l_to_c id_to_breakpoints = function
  | `OrdinalNode {
    on_feature_id;
    on_split;
    on_left_tree;
    on_right_tree;
    on_left_stats;
    on_right_stats;
  } ->
    let breakpoints = Utils.IntMap.find on_feature_id id_to_breakpoints in
    let on_split = breakpoints.( on_split ) in
    let on_left_tree = tree_l_to_c id_to_breakpoints on_left_tree in
    let on_right_tree = tree_l_to_c id_to_breakpoints on_right_tree in
    `OrdinalNode {
      on_feature_id;
      on_split;
      on_left_tree;
      on_right_tree;
      on_left_stats;
      on_right_stats;
    }

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_left_stats;
      cn_right_stats;
    } ->
    let cn_left_tree = tree_l_to_c id_to_breakpoints cn_left_tree in
    let cn_right_tree = tree_l_to_c id_to_breakpoints cn_right_tree in
    let cn_category_directions = rle_of_category_array cn_category_directions in
    `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_left_stats;
      cn_right_stats;
    }

  | (`Leaf _) as leaf -> leaf


let rec add_features_of_tree feat_map map = function
  | `CategoricalNode { cn_feature_id; cn_left_tree; cn_right_tree } ->
    let feature = Feat_map.i_find_by_id feat_map cn_feature_id in
    let map = Utils.IntMap.add cn_feature_id feature map in
    let map = add_features_of_tree feat_map map cn_left_tree in
    let map = add_features_of_tree feat_map map cn_right_tree in
    map

  | `OrdinalNode { on_feature_id; on_left_tree; on_right_tree } ->
    let feature = Feat_map.i_find_by_id feat_map on_feature_id in
    let map = Utils.IntMap.add on_feature_id feature map in
    let map = add_features_of_tree feat_map map on_left_tree in
    let map = add_features_of_tree feat_map map on_right_tree in
    map

  | `Leaf _ -> map

(* as a performance optimization, create a map containing only the
   features referenced by the trees; this is presumeably a (much)
   smaller map that the [feat_map]. *)
let id_to_feature feat_map trees =
  List.fold_left (
    fun map tree ->
      add_features_of_tree feat_map map tree
  ) Utils.IntMap.empty trees

let trees_l_to_c feat_map trees =
  let id_to_feature = id_to_feature feat_map trees in
  let features = Utils.IntMap.fold (
      fun feature_id feature features ->
        (feature_d_to_m feature) :: features
    ) id_to_feature [] in
  let id_to_breakpoints = id_to_breakpoints id_to_feature in

  let trees = List.map (tree_l_to_c id_to_breakpoints) trees in
  trees, features

let fold_l_to_c feat_map fold =
  let trees, features = trees_l_to_c feat_map fold.trees in
  { fold with trees }, features

let folds_l_to_c feat_map folds =
  List.fold_left (fun (accu_folds, accu_features) fold ->
    let fold, features = fold_l_to_c feat_map fold in
    let accu_folds = fold :: accu_folds in
    let accu_features = List.rev_append features accu_features in
    (accu_folds, accu_features)
  ) ([], []) folds

let rec tree_rle_to_array = function
  | (`Leaf _) as leaf -> leaf

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_left_stats;
      cn_right_stats;
    } ->
    let cn_category_directions = category_array_of_rle cn_category_directions in
    let cn_left_tree = tree_rle_to_array cn_left_tree in
    let cn_right_tree = tree_rle_to_array cn_right_tree in
    `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_left_stats;
      cn_right_stats;
    }

  | `OrdinalNode {
    on_feature_id;
    on_split;
    on_left_tree;
    on_right_tree;
    on_left_stats;
    on_right_stats;
  } ->
    let on_left_tree = tree_rle_to_array on_left_tree in
    let on_right_tree = tree_rle_to_array on_right_tree in
    `OrdinalNode {
      on_feature_id;
      on_split;
      on_left_tree;
      on_right_tree;
      on_left_stats;
      on_right_stats;
    }

let trees_rle_to_array trees =
  List.rev_map tree_rle_to_array trees

let fold_rle_to_array fold =
  let open Model_t in
  { fold with trees = trees_rle_to_array fold.trees }

let folds_rle_to_array folds =
  List.rev_map fold_rle_to_array folds

let logistic_rle_to_array bi_model =
  let open Model_t in
  { bi_model with bi_folds = folds_rle_to_array bi_model.bi_folds }

let square_rle_to_array re_model =
  let open Model_t in
  { re_model with re_folds = folds_rle_to_array re_model.re_folds }

let custom_rle_to_array cu_model =
  let open Model_t in
  { cu_model with cu_folds = folds_rle_to_array cu_model.cu_folds }

let rle_to_array : ('a, Model_t.direction_rle, 'c) Model_t.model ->
  ('a, Model_t.category_direction array, 'c) Model_t.model
  =
  function
    | `Logistic bi_model -> `Logistic (logistic_rle_to_array bi_model)
    | `Square re_model -> `Square (square_rle_to_array re_model)
    | `Custom custom_model -> `Custom (custom_rle_to_array custom_model)

let rec tree_extract_stats accu stats (tree : ('a, 'b, 'c) Model_t.tree) =
  match tree with
    | `Leaf _ -> stats :: accu
    | `OrdinalNode on ->
      tree_extract_stats
        (tree_extract_stats accu on.on_left_stats on.on_left_tree)
        on.on_right_stats on.on_right_tree
    | `CategoricalNode cn ->
      tree_extract_stats
        (tree_extract_stats accu cn.cn_left_stats cn.cn_left_tree)
        cn.cn_right_stats cn.cn_right_tree

let convert_cu_point levels_to_values point =
  let { s_gamma; s_n; s_loss } = point in
  let s_gamma = IntMap.find s_gamma levels_to_values in
  { s_gamma;
    s_n;
    s_loss;
  }

let rec convert_cu_tree levels_to_values cu_tree = match cu_tree with
  | (`Leaf gamma) -> `Leaf (IntMap.find gamma levels_to_values)

  | `CategoricalNode {
    cn_feature_id;
    cn_category_directions;
    cn_left_tree;
    cn_right_tree;
    cn_left_stats;
    cn_right_stats;
  } ->
    let cn_left_tree = convert_cu_tree levels_to_values cn_left_tree in
    let cn_right_tree = convert_cu_tree levels_to_values cn_right_tree in
    let cn_left_stats = convert_cu_point levels_to_values cn_left_stats in
    let cn_right_stats = convert_cu_point levels_to_values cn_right_stats in
    `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_left_stats;
      cn_right_stats;
    }

  | `OrdinalNode {
    on_feature_id;
    on_split;
    on_left_tree;
    on_right_tree;
    on_left_stats;
    on_right_stats;
  } ->
    let on_left_tree = convert_cu_tree levels_to_values on_left_tree in
    let on_right_tree = convert_cu_tree levels_to_values on_right_tree in
    let on_left_stats = convert_cu_point levels_to_values on_left_stats in
    let on_right_stats = convert_cu_point levels_to_values on_right_stats in
    `OrdinalNode {
      on_feature_id;
      on_split;
      on_left_tree;
      on_right_tree;
      on_left_stats;
      on_right_stats;
    }

let convert_cu_fold levels_to_values cu_fold =
  let { fold_id; mean; trees } = cu_fold in
  let mean = IntMap.find mean levels_to_values in
  let trees = List.map (convert_cu_tree levels_to_values) trees in
  { fold_id; mean; trees }

let convert_cu_folds levels_to_values cu_folds =
  List.map (convert_cu_fold levels_to_values) cu_folds

let convert_storage_model storage_model : float c_model = match storage_model with
  | `Logistic model -> `Logistic model
  | `Square model -> `Square model
  | `Custom model ->
    let { cu_levels; cu_features; cu_num_folds; cu_folds } = model in
    let levels_to_values : float IntMap.t =
      Array.foldi_left (fun i accu level -> IntMap.add i level accu)
        IntMap.empty cu_levels
    in
    `Custom {
      cu_levels;
      cu_features;
      cu_num_folds;
      cu_folds = convert_cu_folds levels_to_values cu_folds;
    }
