(* a feature map backed by the read-append Dog_io.RW *)

type t = {
  num_observations : int;
  dog_ra : Dog_io.RW.t;
  active_id_to_feature : Dog_io.RW.qfeature Utils.IntMap.t;
  inactive_id_to_feature : Dog_io.RW.qfeature Utils.IntMap.t;
}

let create dog_ra =
{
  dog_ra;
  active_id_to_feature = Utils.IntMap.empty;
  inactive_id_to_feature = Utils.IntMap.empty;
  num_observations = Dog_io.RW.num_observations dog_ra;
}

exception FeatureIdNotFound of Dog_t.feature_id

let add t feature_id vector status =
  if Utils.IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if Utils.IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
    try
      let feature = Dog_io.RW.find t.dog_ra feature_id in
      let () = Dog_io.RW.write t.dog_ra feature_id vector in
      match status with
        | `Active ->
          let active_id_to_feature = Utils.IntMap.add feature_id
              feature t.active_id_to_feature in
          { t with active_id_to_feature }

        | `Inactive ->
          let inactive_id_to_feature = Utils.IntMap.add feature_id
              feature t.inactive_id_to_feature in
          { t with inactive_id_to_feature }
    with (Dog_io.RW.FeatureIdNotFound _) ->
      raise (FeatureIdNotFound feature_id)

let activate t feature_id =
  try
    let feature = Utils.IntMap.find feature_id t.inactive_id_to_feature in
    let active_id_to_feature = Utils.IntMap.add feature_id feature
        t.active_id_to_feature in
    { t with active_id_to_feature }
  with Not_found ->
    if Utils.IntMap.mem feature_id t.active_id_to_feature then
      t (* already active: nothing to do *)
    else
      raise (FeatureIdNotFound feature_id)


let deactivate t feature_id =
  try
    let feature = Utils.IntMap.find feature_id t.active_id_to_feature in
    let inactive_id_to_feature = Utils.IntMap.add feature_id feature
        t.inactive_id_to_feature in
    { t with inactive_id_to_feature }
  with Not_found ->
    if Utils.IntMap.mem feature_id t.inactive_id_to_feature then
      t (* already inactive: nothing to do *)
    else
      raise (FeatureIdNotFound feature_id)

let deactivate_if t f =
  let active_id_to_feature, inactive_id_to_feature = Utils.IntMap.fold (
    fun feature_id feature (active, inactive) ->
      if f feature then
        let inactive = Utils.IntMap.add feature_id feature inactive in
        active, inactive
      else
        let active = Utils.IntMap.add feature_id feature active in
        active, inactive
    ) t.active_id_to_feature (Utils.IntMap.empty, Utils.IntMap.empty)
  in
  { t with active_id_to_feature; inactive_id_to_feature }


let q_find t feature_id =
  try
    Utils.IntMap.find feature_id t.active_id_to_feature
  with Not_found ->
    try
      Utils.IntMap.find feature_id t.inactive_id_to_feature
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

let map_vector t = function
  | `Dense { Dog_io.RW.vector_id } ->
    `Dense {
      Vec.length = t.num_observations;
      array = Dog_io.RW.array t.dog_ra;
      offset = vector_id;
    }

  | `RLE { Dog_io.RW.vector_id } ->
    `RLE {
      Vec.length = t.num_observations;
      array = Dog_io.RW.array t.dog_ra;
      offset = vector_id;
    }

let q_to_a t = function
  | `Cat {
      Dog_t.c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector;
    } ->
    `Cat {
      Dog_t.c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector = map_vector t c_vector;
    }

  | `Ord {
      Dog_t.o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector;
    } ->
    `Ord {
      Dog_t.o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector = map_vector t o_vector;
    }

let a_find_by_id t feature_id =
  let qfeature = q_find t feature_id in
  q_to_a t qfeature

let fold_active t f x0 =
  Utils.IntMap.fold (
    fun feature_id feature x ->
      f (q_to_a t feature) x
  ) t.active_id_to_feature x0

let best_split_of_features t splitter  =
  fold_active t (
    fun feature best_opt ->
      let s_opt = splitter#best_split feature in
      match best_opt, s_opt with
        | Some (_, best_loss, best_split), Some (loss, split) ->

          if best_loss < loss then
            (* still superior *)
            best_opt
          else
            (* new champ *)
            Some (feature, loss, split)

        | None, Some (loss, split) ->
          (* first guy's always champ *)
          Some (feature, loss, split)

        | Some _, None -> best_opt
        | None, None -> None

  ) None

let q_find_all_by_name feature_name map =
  (* since feature names are not unique, we may have multiple features
     satisfying the query *)
  Utils.IntMap.fold (
    fun _ feature features ->
      match Feat_utils.name_of_feature feature with
        | Some fn ->
          if fn = feature_name then
            feature :: features
          else
            features
        | None -> features
  ) map []

let q_find_all_by_name t feature_name =
  (q_find_all_by_name feature_name t.active_id_to_feature) @
    (q_find_all_by_name feature_name t.inactive_id_to_feature)

let a_find_all_by_name t feature_name =
  List.map (q_to_a t) (q_find_all_by_name t feature_name)

let a_find_all t = function
  | `Id feature_id -> [a_find_by_id t feature_id]
  | `Name feature_name -> a_find_all_by_name t feature_name

let num_observations { num_observations } =
  num_observations

let num_active { active_id_to_feature } =
  Utils.IntMap.cardinal active_id_to_feature

let num_inactive { inactive_id_to_feature } =
  Utils.IntMap.cardinal inactive_id_to_feature

  
