(** Friedman's Stochastic Gradient Boosting Trees *)

module IntSet = Utils.IntSet

let random_seed = [| 9271 ; 12074; 3; 12921; 92; 763 |]

type loss_type = [ `Logistic | `Square ]

type feature_monotonicity = (Feat_utils.feature_descr * Dog_t.monotonicity) list
type conf = {
  loss_type : loss_type;
  dog_file_path : string;
  num_folds : int;
  only_fold_id : int option;
  min_observations_per_node : float;
  min_convergence_rate : float;
  initial_learning_rate : float;
  ys : Feat_utils.feature_descr list;
  max_depth : int;
  convergence_rate_smoother_forgetful_factor : float;
  deadline : float option;
  output_file_path : string;
  selected_feature_name_regexp_opt : Pcre.regexp option;
  excluded_feature_name_regexp_opt : Pcre.regexp option;
  fold_feature_opt : Feat_utils.feature_descr option;
  weight_feature_opt : Feat_utils.feature_descr option;
  max_trees_opt : int option;
  max_gamma_opt : float option;
  max_features_opt : int option;
  binarization_threshold_opt : Logistic.binarization_threshold option;
  feature_monotonicity : feature_monotonicity;
  exclude_nan_target : bool;
  exclude_inf_target : bool;
  stochastic_gradient : bool;
  best_split_algo : [ `Fibonacci | `Exhaustive ];
}

type t = {
  (* Unix.gettimeofday() *)
  start_time : float;

  (* how many observations are there in the training set? *)
  n_rows : int;

  (* what is the map of feature id's to features? *)
  feature_map : Feat_map.t;

  (* how do we find best splits? *)
  splitter : Loss.splitter;

  (* how do we evaluate a tree over all the observations in the
     training set? *)
  eval : (Dog_t.feature_id -> Feat.afeature) -> Model_t.l_tree -> float array;

  (* how do we create random paritions of the training set (and
     subsets thereof) ? *)
  sampler : Sampler.t;

  (* what fold does each observation in the training set belong
     to? *)
  folds : int array;
}

let exceed_max_trees num_iters max_trees = num_iters >= max_trees

let reset t mean_model =
  let first_tree = `Leaf mean_model in
  let gamma = t.eval (Feat_map.a_find_by_id t.feature_map) first_tree in
  t.splitter#clear;
  (match t.splitter#boost gamma with
    | `NaN _ -> assert false
    | `Ok -> ()
  )

type learning_iteration = {
  (* iteration number; also, number of trees *)
  i : int ;

  (* what is the fold currently being held out for the purpose of
     identifying the optimal termination point? This fold is the
     validation fold. *)
  fold_id : int;
  fold_start_time : float; (* Unix.gettimeofday() *)
  tree_start_time : float;

  (* is the observation in the 'working folds' or the 'validation
     fold' ?  *)
  in_set : bool array;
  out_set : bool array;

  learning_rate : float;
  first_loss : float;
  prev_loss : float;
  allowed_features : Feat_map.t option;
  rev_trees : Model_t.l_tree list;
  selected_features : IntSet.t;

  convergence_rate_smoother : Rls1.t;
  random_state : Random.State.t;

  (* what is the first tree? *)
  mean_model : float;

  (* should learning stop, returning the best model produced so
     far? *)
  timeout : unit -> bool
}

let string_of_stats { Proto_t.s_gamma; s_n; s_loss } =
  Printf.sprintf "{ s_gamma=%f; s_n=%f; s_loss=%f }"
    s_gamma s_n s_loss

let fake_stats = { Proto_t.s_gamma = nan; s_n = nan; s_loss = nan }

let (~%) f y x = f x y

let rec learn_with_fold_rate conf t iteration =
  let feature_monotonicity_map =
    Feat_map.assoc t.feature_map conf.feature_monotonicity
  in
  let feature_map = match iteration.allowed_features with
    | None -> t.feature_map
    | Some x -> x
  in
  let m = {
    Tree.max_depth = conf.max_depth;
    feature_map;
    feature_monotonicity_map;
    splitter = t.splitter;
  } in

  (* draw a random subset of this fold *)
  Sampler.shuffle t.sampler iteration.random_state;
  let { in_set; out_set } = iteration in
  let in_subset =
    if conf.stochastic_gradient then
      Sampler.array (
        fun ~index ~value ->
          (* sample half the data that is also in the current fold *)
          in_set.(index) && value mod 2 = 0
      ) t.sampler
    else
      in_set
  in
  let converge iteration =
    let fold = {
      Model_t.fold_id = iteration.fold_id;
      mean = iteration.mean_model;
      trees = List.rev iteration.rev_trees;
    } in
    let now = Unix.gettimeofday() in
    Utils.epr "%6.0fs %6.2fs fold %i converged\n%!"
      (now -. t.start_time)
      (now -. iteration.fold_start_time)
      iteration.fold_id;

    `Converged (iteration.learning_rate, fold)
  in

  match Tree.make m 0 in_subset with
    | None ->
      print_endline "converged: no more trees";
      converge iteration

    | Some tree ->
      let tree = Tree.scale_optimally tree in_subset in
      let shrunken_tree = Tree.shrink iteration.learning_rate tree in
      let gamma = t.eval (Feat_map.a_find_by_id t.feature_map) shrunken_tree in

      match t.splitter#boost gamma with
        | `NaN i -> (
            Utils.epr "[WARNING] diverged: nan at row %d\n%!" i;
            cut_learning_rate conf t iteration
          )

        | `Ok ->
          let { Loss.s_wrk; s_val; has_converged; val_loss } =
            t.splitter#metrics ~in_set ~out_set in

          (* compute convergence rate and smooth it *)
          let convergence_rate =
            (iteration.prev_loss -. val_loss) /. val_loss in
          let convergence_rate_smoother = Rls1.add
              iteration.convergence_rate_smoother convergence_rate in
          let convergence_rate_hat = Rls1.theta convergence_rate_smoother in
          let now = Unix.gettimeofday () in
          Utils.pr "%6.0fs %6.2fs iter % 3d % 7d  %s %s    %+.4e %+.4e\n%!"
            (now -. iteration.fold_start_time)
            (now -. iteration.tree_start_time)
            iteration.fold_id
            iteration.i
            s_wrk
            s_val
            convergence_rate
            convergence_rate_hat;

          let selected_features =
            IntSet.union iteration.selected_features (Tree.extract_features tree)
          in
          let allowed_features =
            match conf.max_features_opt with
              | None -> None
              | Some max_features ->
                match iteration.allowed_features with
                  | (Some _) as x -> x
                  | None ->
                    if IntSet.cardinal selected_features > max_features then
                      Some (Feat_map.restrict t.feature_map selected_features)
                    else
                      None
          in
          let next_iteration () =
            (* let stats = Model_utils.tree_extract_stats [] fake_stats tree in *)
            (* Utils.epr "[DEBUG] i=%d %s\n%!" *)
            (*   iteration.i *)
            (*   (String.concat " " (List.rev_map string_of_stats stats)); *)
            { iteration with
              prev_loss = val_loss;
              i = iteration.i + 1;
              tree_start_time = Unix.gettimeofday();
              rev_trees = shrunken_tree :: iteration.rev_trees;
              convergence_rate_smoother;
              selected_features;
              allowed_features;
            }
          in
          let continue_learning () =
            learn_with_fold_rate conf t (next_iteration ())
          in

          if iteration.timeout () then (
            Utils.pr "timeout!\n";
            let fold = {
              Model_t.fold_id = iteration.fold_id;
              mean = iteration.mean_model;
              trees = shrunken_tree :: iteration.rev_trees;
            }
            in
            `Timeout fold
          ) else
            match conf.max_trees_opt with
              | Some max_trees ->
                if exceed_max_trees iteration.i max_trees then (
                  (* convergence, kinda *)
                  Utils.pr "tree limit constraint met\n";
                  converge (next_iteration ())
                )
                else continue_learning ()

              | None ->
                if has_converged then (
                  Utils.pr "converged: metrics indicate continuing is pointless\n";
                  converge (next_iteration ())
                )
                else if val_loss >= 2.0 *. iteration.prev_loss then (
                  Utils.pr "diverged: loss rose dramatically!\n";
                  cut_learning_rate conf t iteration
                )
                else if convergence_rate_hat < conf.min_convergence_rate then (
                  (* convergence! *)
                  Utils.pr "converged: rate exceeded\n";
                  if val_loss >= iteration.prev_loss then
                    converge iteration
                  else
                    converge (next_iteration ())
                )
                else continue_learning ()


and cut_learning_rate conf t iteration =
  (* cut the learning rate in half and try again *)
  let learning_rate = 0.5 *. iteration.learning_rate in
  Utils.pr "reducing learning rate from %f to %f\n"
    iteration.learning_rate learning_rate;
  reset t iteration.mean_model;
  let new_random_seed = [| Random.int 10_000 |] in
  let iteration = {
    iteration with
      learning_rate;
      random_state = Random.State.make new_random_seed;
      prev_loss = iteration.first_loss;
      i = 1;
      tree_start_time = Unix.gettimeofday();
      rev_trees = [];
  } in
  learn_with_fold_rate conf t iteration

let learn_with_fold conf t fold_id initial_learning_rate deadline =
  let in_set =
    if conf.num_folds = 1 then
      Array.init t.n_rows (fun i -> let n = t.folds.(i) in n >= 0)
    else
      Array.init t.n_rows (fun i -> let n = t.folds.(i) in n >= 0 && n <> fold_id)
  in
  let out_set =
    Array.init t.n_rows (fun i -> let n = t.folds.(i) in n >= 0 && n = fold_id)
  in

  let mean_model = t.splitter#mean_model in_set in
  reset t mean_model;

  let { Loss.s_wrk; s_val; val_loss = first_val_loss } =
    t.splitter#metrics ~in_set ~out_set
  in

  Utils.pr "% 15s fold % 3d          %s %s\n%!" "" fold_id s_wrk s_val;

  let new_random_seed = [| Random.int 10_000 |] in

  let timeout =
    match conf.deadline with
      | None -> fun () -> false (* don't timeout *)
      | Some deadline ->
        fun () ->
          Unix.gettimeofday () >= deadline (* obey deadline *)
  in

  let convergence_rate_smoother = Rls1.create
      conf.convergence_rate_smoother_forgetful_factor in

  let now = Unix.gettimeofday() in
  let iteration = {
    i = 1;
    fold_start_time = now;
    tree_start_time = now;

    fold_id;
    in_set;
    out_set;
    first_loss = first_val_loss;
    prev_loss = first_val_loss;
    mean_model;
    rev_trees = [];
    allowed_features = None;
    selected_features = IntSet.empty;
    learning_rate = initial_learning_rate;
    convergence_rate_smoother;
    random_state = Random.State.make new_random_seed;
    timeout;
  } in

  learn_with_fold_rate conf t iteration

let folds_and_weights conf sampler feature_map n =
  let fold_feature_id, folds, feature_map = match conf.fold_feature_opt with
    | None ->
      (* randomly choose fold assignments *)
      let folds = Sampler.array (
          fun ~index ~value ->
            value mod conf.num_folds
        ) sampler in
      None, folds, feature_map

    | Some fold_feature ->

      match Feat_map.find feature_map fold_feature with
        | [] ->
          Utils.epr "[ERROR] feature %S to be used for fold assignment not found\n%!"
            (Feat_utils.string_of_feature_descr fold_feature);
          exit 1

        | fold_features ->
          let num_fold_features = List.length fold_features in
          if num_fold_features > 1 then
            Utils.pr "[WARNING] There are %d fold features satisfying %s\n%!"
              num_fold_features
              (Feat_utils.string_of_feature_descr fold_feature);

          (* arbitrarily pick the first fold feature (should there be
             more than one) *)
          let i_fold_feature = List.hd fold_features in
          let fold_feature_id = Feat_utils.id_of_feature i_fold_feature in

          (* fold feature found; use it to construct folds *)
          let a_fold_feature = Feat_map.i_to_a feature_map i_fold_feature in
          match Feat_utils.folds_of_feature ~n ~num_folds:conf.num_folds
                  a_fold_feature with
            | `TooManyCategoricalFolds cardinality ->
              Utils.epr "[ERROR] The cardinality of categorical feature %s is %d, which is \
                  too small relative to the number of folds %d\n%!"
                (Feat_utils.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `TooManyOrdinalFolds cardinality ->
              Utils.epr "[ERROR] The cardinality of ordinal feature %s is %d, which is \
                  too small relative to the number of folds %d\n%!"
                (Feat_utils.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `Folds folds ->
              (* remove all the fold_features from the [feature_map] *)
              let feature_map =
                List.fold_left (
                  fun feature_map_0 a_fold_feature ->
                    let fold_feature_id = Feat_utils.id_of_feature
                      a_fold_feature in
                    let fold_feature_name = Feat_utils.name_of_feature a_fold_feature in
                    let () = match fold_feature_name with
                      | Some name ->
                        Utils.epr "[INFO] excluding fold feature %s (id: %d)\n%!"
                          name fold_feature_id;
                      | None ->
                        Utils.epr "[INFO] excluding nameless fold feature (id: %d)\n%!"
                          fold_feature_id;
                    in
                    Feat_map.remove feature_map_0 fold_feature_id
                ) feature_map fold_features  in
             Some fold_feature_id, folds, feature_map
  in

  let weights = match conf.weight_feature_opt with
    | None ->
      (* all weights are set to 1.0 *)
      let weights = Sampler.array (
          fun ~index ~value ->
            1.0
        ) sampler in
      weights

    | Some weight_feature ->
      match Feat_map.find feature_map weight_feature with
        | [] ->
          Utils.epr "[ERROR] feature %S to be used for observation weights not found\n%!"
            (Feat_utils.string_of_feature_descr weight_feature);
          exit 1

        | weight_features ->
          let num_weight_features = List.length weight_features in
          if num_weight_features > 1 then (
            Utils.epr "[ERROR] There are %d weight features satisfying %s\n%!"
              num_weight_features
              (Feat_utils.string_of_feature_descr weight_feature);
            exit 1
          );

          (* there is exactly one weight feature) *)
          let i_weight_feature = List.hd weight_features in

          (* weight feature found; use it *)
          let a_weight_feature = Feat_map.i_to_a feature_map i_weight_feature in
          let weights = Feat_utils.weights_of_afeature a_weight_feature in
          Array.iteri (fun i w ->
            match classify_float w with
              | FP_infinite ->
                Utils.epr "[ERROR] Infinite weight for observation %d: %f" i w;
                exit 2
              | FP_nan ->
                Utils.epr "[ERROR] NaN weight for observation %d" i;
                exit 2
              | FP_zero -> ()
              | _ when w < 0.0 ->
                Utils.epr "[ERROR] Negative weight for observation %d: %f" i w;
                exit 2
              | _ -> ()
          ) weights;

          weights
  in

  fold_feature_id, folds, weights, feature_map

let extract_y_features ~fold_feature_id feature_map ys =
  List.fold_right (fun y accu ->
    match Feat_map.find feature_map y with
      | [] ->
        Utils.epr "[ERROR] target %s not found\n%!"
          (Feat_utils.string_of_feature_descr y);
        exit 1

      | i_ys ->
        List.fold_right (fun i_y_feature (i_y_features, a_y_features, feature_map) ->
          let a_y_feature = Feat_map.i_to_a feature_map i_y_feature in
          let y_feature_id = Feat_utils.id_of_feature a_y_feature in
          let () = match fold_feature_id with
            | Some id when id = y_feature_id  ->
              Utils.epr "[ERROR] Fold feature and target feature must be different\n%!";
              exit 1
            | _ -> ()
          in
          let i_y_features = i_y_feature :: i_y_features in
          let a_y_features = a_y_feature :: a_y_features in
          let feature_map = Feat_map.remove feature_map (Feat_utils.id_of_feature i_y_feature) in
          i_y_features, a_y_features, feature_map
        ) i_ys accu
  ) ys ([], [], feature_map)


let learn conf =
  let dog_reader = Dog_io.RO.create conf.dog_file_path in
  let feature_map = Feat_map.create dog_reader in

  let n_rows =
    let dog = Dog_io.RO.dog dog_reader in
    dog.Dog_t.num_observations
  in
  (* let n = num_observations in *)

  assert ( conf.num_folds > 0 );
  if conf.num_folds >= n_rows then (
    Utils.epr "[ERROR] number of folds %d must be smaller than the number of observations \
        %d\n%!" conf.num_folds n_rows;
    exit 1
  );


  let random_state = Random.State.make random_seed in
  let sampler = Sampler.create n_rows in
  Sampler.shuffle sampler random_state;

  let fold_feature_id, folds, weights, feature_map =
    folds_and_weights conf sampler feature_map n_rows
  in
  let i_y_features, a_y_features, feature_map =
    extract_y_features ~fold_feature_id feature_map conf.ys
  in

  let num_all_features = Feat_map.length feature_map in
  let feature_map =
    (* remove not-selected features, if any *)
    let feature_map =
      match conf.selected_feature_name_regexp_opt with
        | None -> feature_map
        | Some rex ->
          let is_selected feature =
            match Feat_utils.name_of_feature feature with
            | None -> false (* anonymous features cannot be excluded *)
            | Some name -> Pcre.pmatch ~rex name
          in
          let is_included _ feature =
            is_selected feature
          in
          Feat_map.filter feature_map is_included
    in

    (* remove excluded features, if any *)
    let feature_map =
      match conf.excluded_feature_name_regexp_opt with
        | None -> feature_map
        | Some rex ->
          let is_excluded feature =
            match Feat_utils.name_of_feature feature with
            | None -> false (* anonymous features cannot be excluded *)
            | Some name -> Pcre.pmatch ~rex name
          in
          let is_included _ feature =
            not (is_excluded feature)
          in
          Feat_map.filter feature_map is_included
    in
    feature_map
  in
  let num_features = Feat_map.length feature_map in

  let exclude_set = Array.init n_rows (fun i -> folds.(i) < 0) in
  let excluded_observations =
    Array.fold_left (fun n b -> if b then succ n else n) 0 exclude_set
  in
  Array.iteri (fun i ex -> if ex then weights.(i) <- 0.0) exclude_set;
  let num_observations = n_rows - excluded_observations in

  assert (
    (* make sure fold features (if any) are gone from the
       [feature_map] *)
    match conf.fold_feature_opt with
      | Some fold_feature -> (
          let ff = Feat_map.find feature_map fold_feature in
          match ff with
            | [] -> true
            | _ -> false
        )
      | None -> true
  );

  Utils.pr "features: all=%d included=%d excluded=%d\n%!"
    num_all_features num_features (num_all_features - num_features);

  let minimize = match conf.best_split_algo with
    | `Fibonacci -> Fibsearch.minimize
    | `Exhaustive -> Fibsearch.minimize_exhaustive
  in

  let splitter : Loss.splitter =
    match conf.loss_type with
      | `Logistic ->
        let y_feature = match a_y_features with
          | [] ->
            Utils.epr "[ERROR] no target feature for logistic model";
            exit 1
          | [y_feature] -> y_feature
          | _ :: _ :: _ ->
            Utils.epr "[ERROR] too many features for logistic model";
            Utils.epr "[ERROR] currently logistic models support only 1 target";
            exit 1
        in
        new Logistic.splitter
          ~minimize
          ~max_gamma_opt:conf.max_gamma_opt
          ~binarization_threshold_opt:conf.binarization_threshold_opt
          ~weights
          ~y_feature
          ~n_rows
          ~num_observations
          ~min_observations_per_node:conf.min_observations_per_node
      | `Square ->
        let y_feature = match a_y_features with
          | [] ->
            Utils.epr "[ERROR] no target feature for square error model";
            exit 1
          | [y_feature] -> y_feature
          | _ :: _ :: _ ->
            Utils.epr "[ERROR] too many features for squre error model";
            Utils.epr "[ERROR] currently square error models support only 1 target";
            exit 1
        in
        new Square.splitter
          ~minimize
          ~max_gamma_opt:conf.max_gamma_opt
          ~weights
          ~y_feature
          ~n_rows
          ~num_observations
          ~min_observations_per_node:conf.min_observations_per_node
  in

  let eval = Tree.mk_eval n_rows in

  let t = {
    start_time = Unix.gettimeofday();
    n_rows;
    feature_map;
    splitter;
    eval;
    sampler;
    folds
  } in

  let rec loop fold_id (folds : ('a, 'b) Model_t.folds) initial_learning_rate =
    if (match conf.only_fold_id with
      | None -> fold_id >= conf.num_folds
      | Some i -> fold_id <> i
    ) then
      folds
    else
      match learn_with_fold conf t fold_id initial_learning_rate 0.0 with

        | `Converged (effective_learning_rate, fold) ->
          let folds = fold :: folds in
          (* set the initial learning rate of the next fold model to the
             effective learning rate of the previous one; this means that
             the learning rate can gradually decrease from one fold to the
             next, as a learning attempt on folds fail (diverge) *)
          loop (fold_id + 1) folds effective_learning_rate

        | `Timeout fold ->
          (* time's up!  only include [trees] if no other trees were
             previously learned. *)
          match folds with
            | [] -> [fold]
            | _ -> folds

  in

  (* combine the model learned for each fold into a mega-model,
     where these sequence of trees are simply averaged (bagged!) *)
  let initial_fold_id = match conf.only_fold_id with
    | Some i -> i
    | None -> 0
  in
  let folds = loop initial_fold_id [] conf.initial_learning_rate in

  let folds, features = Model_utils.folds_l_to_c feature_map folds in

  (* write model file *)
  let () =
    (* open channel *)
    let ouch = open_out conf.output_file_path in

    (* open output buffer *)
    let out_buf = Bi_outbuf.create_channel_writer ouch in

    (* write model to buffer *)
    splitter#write_model folds features out_buf;

    (* flush buffer *)
    Bi_outbuf.flush_channel_writer out_buf;

    (* close channel *)
    close_out ouch
  in
  ()
