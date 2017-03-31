open Dog_t

module List = Utils.List

let pr = Printf.printf
let epr = Printf.eprintf

let apply_max_gamma ~max_gamma gamma =
  if gamma < 0.0 then
    max gamma (~-. max_gamma)
  else
    min gamma max_gamma

let apply_max_gamma_opt ~max_gamma_opt left right =
  match max_gamma_opt with
    | None -> left, right
    | Some max_gamma ->
      apply_max_gamma ~max_gamma left, apply_max_gamma ~max_gamma right

let repr_of_afeature = function
  | `Cat cat -> (
      let categories = Array.of_list cat.c_categories in
      let num_categories = Array.length categories in
      match cat.c_anonymous_category with
        | Some anon_value -> (

            let num_categories = num_categories + 1 in

            let string_opt_of_int value =
              if 0 <= value && value < anon_value then
                Some categories.( value )
              else if value = anon_value then (* anonymous *)
                None
              else if anon_value < value && value < num_categories then
                Some categories.( value - 1 )
              else
                assert false
            in
            match cat.c_vector with
              | `RLE (rle:Vec.t) ->
                let result = Array.make rle.Vec.length (-1, None) in
                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let string_opt = value, string_opt_of_int value in
                    for i = index to index + length - 1 do
                      result.(i) <- string_opt
                    done
                );
                `StringAnon result

              | `Dense (vec:Vec.t) ->
                let result = Array.make vec.Vec.length (-1, None) in
                let width = Utils.num_bytes cat.c_cardinality in
                Dense.iter ~width vec (
                  fun ~index ~value ->
                    let string_opt = string_opt_of_int value in
                    result.(index) <- value, string_opt
                );
                `StringAnon result
          )
        | None -> (
            let category_0 = List.hd cat.c_categories in
            match cat.c_vector with
              | `RLE rle ->
                let result = Array.make rle.Vec.length (-1, category_0) in
                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, categories.( value ) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done

                );
                `String result

              | `Dense vec ->
                let result = Array.make vec.Vec.length (-1, category_0) in
                let width = Utils.num_bytes cat.c_cardinality in
                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.(index) <- value, categories.( value )
                );
                `String result
          )
    )

  | `Ord { o_vector; o_breakpoints; o_cardinality } -> (
      match o_vector with
        | `RLE rle -> (
            match o_breakpoints with
              | `Float breakpoints ->
                let result = Array.make rle.Vec.length (-1, 0.0) in
                let repr_elements = breakpoints.repr_elements in
                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, repr_elements.(value) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done
                );
                `Float result

              | `Int breakpoints ->
                let result = Array.make rle.Vec.length (-1, 0) in
                let repr_elements = breakpoints.repr_elements in

                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, repr_elements.(value) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done
                );
                `Int result
          )
        | `Dense vec -> (
            let width = Utils.num_bytes o_cardinality in
            match o_breakpoints with
              | `Float breakpoints ->
                let result = Array.make vec.Vec.length (-1, 0.0) in
                let repr_elements = breakpoints.repr_elements in
                assert (o_cardinality = Array.length repr_elements);

                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.(index) <- value, repr_elements.(value)
                );
                `Float result

              | `Int breakpoints ->
                let result = Array.make vec.Vec.length (-1, 0) in
                let repr_elements = breakpoints.repr_elements in
                assert (o_cardinality = Array.length repr_elements);

                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.(index) <- value, repr_elements.(value)
                );
                `Int result
          )
    )


let id_of_feature = function
  | `Cat { c_feature_id } -> c_feature_id
  | `Ord { o_feature_id } -> o_feature_id

let name_of_feature = function
  | `Cat { c_feature_name_opt } -> c_feature_name_opt
  | `Ord { o_feature_name_opt } -> o_feature_name_opt

let cardinality_of_feature = function
  | `Cat { c_cardinality } -> c_cardinality
  | `Ord { o_cardinality } -> o_cardinality

let vector_of_feature = function
  | `Cat { c_vector } -> c_vector
  | `Ord { o_vector } -> o_vector

let folds_of_feature ~n ~num_folds = function
  | `Ord { o_cardinality; o_vector } ->
    assert ( o_cardinality <= n );
    if o_cardinality < num_folds then
      `TooManyOrdinalFolds o_cardinality
    else
    let cardinality_per_fold = float o_cardinality /. float num_folds in
      let folds = Array.make n (-1) in
      (match o_vector with
        | `RLE rle ->
          Rlevec.iter rle (
            fun ~index ~length ~value ->
              let fold = Utils.ifloor (float value /. cardinality_per_fold) in
              for i = index to index + length - 1 do
                folds.(i) <- fold
              done
          );

        | `Dense vec ->
          let width_num_bytes = Utils.num_bytes o_cardinality in
          Dense.iter ~width:width_num_bytes vec (
            fun ~index ~value ->
              let fold = Utils.ifloor (float value /. cardinality_per_fold) in
              folds.(index) <- fold
          );
      );
      `Folds folds

  | `Cat { c_cardinality; c_vector } ->
    if c_cardinality < num_folds then
      `TooManyCategoricalFolds c_cardinality
    else
      let folds = Array.make n (-1) in
      (match c_vector with
        | `RLE rle ->
          Rlevec.iter rle (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                folds.(i) <- value mod num_folds
              done
          );

        | `Dense vec ->
          let width_num_bytes = Utils.num_bytes c_cardinality in
          Dense.iter ~width:width_num_bytes vec (
            fun ~index ~value ->
              folds.(index) <- value mod num_folds
          )
      );
      `Folds folds

let weights_of_afeature = function
  | `Cat { c_feature_name_opt = Some feature_name } ->
    epr "[ERROR] feature %s is categorical but a float feature is needed" feature_name;
    exit 1
  | `Cat { c_feature_id = feature_id } ->
    epr "[ERROR] nameless feature id %d is categorical but a float feature is needed" feature_id;
    exit 1
  | afeature ->
    match repr_of_afeature afeature with
      | `String _ -> assert false
      | `StringAnon _ -> assert false
      | `Int id_weight_array ->
        Array.map (fun (id, w_int) -> float w_int) id_weight_array
      | `Float id_weight_array ->
        Array.map (fun (id, w) -> w) id_weight_array

let vector f = function
  | `RLE rle -> `RLE (f rle)
  | `Dense dense -> `Dense (f dense)


let i_to_a f = function
  | `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector;
    } ->
    `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector = vector f c_vector;
    }

  | `Ord {
      o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector;
    } ->
    `Ord {
      o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector = vector f o_vector;
    }

type feature_descr = [ `Name of string | `Id of int ]

let string_of_feature_descr = function
  | `Name name -> Printf.sprintf "name:%s" name
  | `Id id -> Printf.sprintf "id:%d" id

let feature_descr_of_string = function
  | RE "name:" (_+ as name) -> Some (`Name name)
  | RE "id:" (int as id) -> Some (`Id (int_of_string id))
  | _ -> None

let descr_of_feature feature =
  match feature with
    | `Cat { c_feature_name_opt = name_opt; c_feature_id = id }
    | `Ord { o_feature_name_opt = name_opt; o_feature_id = id } ->
      match name_opt with
        | Some name -> `Name name
        | None -> `Id id

let string_descr_of_feature feature =
  string_of_feature_descr (descr_of_feature feature)

let descr_of_ord_feature feature =
  match feature with
    | { o_feature_name_opt = name_opt; o_feature_id = id } ->
      match name_opt with
        | Some name -> `Name name
        | None -> `Id id

let descr_of_cat_feature feature =
  match feature with
    | { c_feature_name_opt = name_opt; c_feature_id = id } ->
      match name_opt with
        | Some name -> `Name name
        | None -> `Id id

let iter_ord_feature f y_feature =
  let open Dog_t in
  let { o_vector; o_breakpoints; o_cardinality } = y_feature in
  match o_vector with
    | `RLE vec -> (
      match o_breakpoints with
        | `Float breakpoints ->
          let repr_elements = breakpoints.repr_elements in
          Rlevec.iter vec (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                f i repr_elements.(value)
              done
          )

        | `Int breakpoints ->
          let repr_elements = breakpoints.repr_elements in
          Rlevec.iter vec (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                f i (float repr_elements.(value))
              done
          )
    )

    | `Dense vec -> (
      let width = Utils.num_bytes o_cardinality in
      match o_breakpoints with
        | `Float breakpoints ->
          let repr_elements = breakpoints.repr_elements in
          Dense.iter ~width vec (
            fun ~index ~value ->
              f index repr_elements.(value)
          )

        | `Int breakpoints ->
          let repr_elements = breakpoints.repr_elements in
          Dense.iter ~width vec (
            fun ~index ~value ->
              f index (float repr_elements.(value))
          )
    )

let iter_ord_by_level f y_feature =
  let open Dog_t in
  let { o_vector; o_breakpoints; o_cardinality } = y_feature in
  match o_vector with
    | `RLE vec -> (
      match o_breakpoints with
        | `Float breakpoints ->
          Rlevec.iter vec (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                f i value
              done
          )

        | `Int breakpoints ->
          Rlevec.iter vec (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                f i value
              done
          )
    )

    | `Dense vec -> (
      let width = Utils.num_bytes o_cardinality in
      match o_breakpoints with
        | `Float breakpoints ->
          Dense.iter ~width vec (
            fun ~index ~value ->
              f index value
          )

        | `Int breakpoints ->
          Dense.iter ~width vec (
            fun ~index ~value ->
              f index value
          )
    )

let repr_array_of_ord_feature n_obs y_feature =
  let open Dog_t in
  let y = Array.make n_obs nan in
  iter_ord_feature (Array.set y) y_feature;
  assert (
    try
      for i = 0 to Array.length y - 1 do
        match classify_float y.(i) with
          | FP_nan ->
            Printf.printf "y.(%d)=%f\n%!" i y.(i);
            raise Sys.Break
          | _ -> ()
      done;
      true
    with Sys.Break ->
      false
  );
  y

let repr_table_of_ord_features ~scale n_obs y_features =
  let m = List.length y_features in
  let ys = Array.init n_obs (fun _ -> Array.make m nan) in
  (* ys is a column-major matrix: we want every row to be contiguous in memory *)

  List.iteri (fun col feature ->
    iter_ord_feature (fun row value ->
      (* Utils.epr "col=%d/%d row=%d/%d\n%!" col (Array.length ys) row (Array.length ys.(0)); *)
      ys.(row).(col) <- scale *. value
    ) feature
  ) y_features;
  ys

let repr_elements_of_ord_feature y_feature =
  let open Dog_t in
  let { o_vector; o_breakpoints; o_cardinality } = y_feature in
  match o_breakpoints with
    | `Float breakpoints -> breakpoints.repr_elements
    | `Int breakpoints -> Array.map float breakpoints.repr_elements

let histogram ord_feature ~in_set ~weights =
  let { o_cardinality } = ord_feature in
  let nn = ref 0.0 in
  let hist_array = Array.make o_cardinality 0.0 in
  iter_ord_by_level (fun i level ->
    if in_set.(i) then
      let wi = weights.(i) in
      hist_array.(level) <- hist_array.(level) +. wi;
      Utils.add_to nn wi
  ) ord_feature;

  let repr_elements = repr_elements_of_ord_feature ord_feature in
  let sum_n = !nn in
  { Utils.Stats.repr_elements; hist_array; sum_n }
