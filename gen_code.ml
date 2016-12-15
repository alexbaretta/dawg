let rec foldi_left : (int -> 'a -> 'b -> 'a) -> int -> 'a -> 'b list -> 'a =
  fun f i accu l ->
    match l with
    | hd :: tl ->
      let accu = f i accu hd in
      foldi_left f (succ i) accu tl
    | [] -> accu

open Model_t
let sp = Printf.sprintf

module RLEMap = Utils.XMap(
    struct
      type t = direction_rle
      let compare = Pervasives.compare
    end
  )

let logistic_mean_value bi_model =
  let open Model_t in
  let total =
    List.fold_left (fun accu fold -> accu +. fold.mean) 0.0 bi_model.bi_folds
  in
  total /. (float (List.length bi_model.bi_folds))

let square_mean_value re_model =
  let open Model_t in
  let total =
    List.fold_left (fun accu fold -> accu +. fold.mean) 0.0 re_model.re_folds
  in
  total /. (float (List.length re_model.re_folds))

let mean_value = function
  | `Logistic bi_model -> logistic_mean_value bi_model
  | `Square re_model -> square_mean_value re_model

(* assign to each category_directions node a variable id, which will
   be bound to an boolean list *)
let rec category_direction_ids_of_tree next_id category_directions_to_id =
  function
    | `OrdinalNode {
        on_feature_id;
        on_split;
        on_left_tree;
        on_right_tree;
        on_left_stats;
        on_right_stats;
    } ->
      let on_left_tree, next_id, category_directions_to_id =
        category_direction_ids_of_tree next_id category_directions_to_id
          on_left_tree in
      let on_right_tree, next_id, category_directions_to_id =
        category_direction_ids_of_tree next_id category_directions_to_id
          on_right_tree in
      `OrdinalNode {
        on_feature_id;
        on_split;
        on_left_tree;
        on_right_tree;
        on_left_stats;
        on_right_stats;
      }, next_id, category_directions_to_id

    | `CategoricalNode {
        cn_feature_id;
        cn_category_directions;
        cn_left_tree;
        cn_right_tree;
        cn_left_stats;
        cn_right_stats;
    } ->

      let category_directions_to_id, cn_category_directions, next_id =
        try
          let id = RLEMap.find cn_category_directions
              category_directions_to_id in
          category_directions_to_id, id, next_id
        with Not_found ->
          RLEMap.add cn_category_directions next_id category_directions_to_id,
          next_id, next_id + 1
      in

      let cn_left_tree, next_id, category_directions_to_id =
        category_direction_ids_of_tree next_id category_directions_to_id
          cn_left_tree in
      let cn_right_tree, next_id, category_directions_to_id =
        category_direction_ids_of_tree next_id category_directions_to_id
          cn_right_tree in
      `CategoricalNode {
        cn_feature_id;
        cn_category_directions;
        cn_left_tree;
        cn_right_tree;
        cn_left_stats;
        cn_right_stats;
      }, next_id, category_directions_to_id

    | `Leaf leaf -> `Leaf leaf, next_id, category_directions_to_id

let category_direction_ids_of_trees trees =
  let trees, next_id, category_directions_to_id = List.fold_left (
      fun (trees, next_id, category_directions_to_id) tree ->
        let tree, next_id, category_directions_to_id =
          category_direction_ids_of_tree next_id category_directions_to_id
            tree in
        let trees = tree :: trees in
        trees, next_id, category_directions_to_id
    ) ([], 0, RLEMap.empty) trees in
  trees, category_directions_to_id
