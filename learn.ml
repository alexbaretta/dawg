module IntMap = Utils.IntMap
module IntSet = Utils.IntSet
module SSet = Utils.XSet(String)

let pr = Utils.pr
let epr = Utils.epr

let split_args s = Pcre.split ~pat:"[ \n\r\t,]" s

let seconds_of_string = function
  | RE (float as number : float ) ( 's' | 'S' ) -> (* seconds *)
    Some number

  | RE (float as number : float ) ( 'm' | 'M' ) -> (* minutes *)
    Some (number *. 60.)

  | RE (float as number : float ) ( 'h' | 'H' ) -> (* hours *)
    Some (number *. 60. *. 60.)

  | RE (float as number : float ) ( 'd' | 'D' ) -> (* days *)
    Some (number *. 60. *. 60. *. 24.)

  | _ -> None

let deadline_of_string str =
  match seconds_of_string str with
    | None ->
      epr "[ERROR] %S is not a valid time-delta sepcifier\n%!" str;
      exit 1

    | Some delta_seconds ->
      let now = Unix.gettimeofday () in
      now +. delta_seconds

let feature_descr_of_name name = `Name name
let feature_descr_of_id id = `Id id

let feature_descr_of_arg name_opt id_opt =
  match name_opt, id_opt with
    | None, None -> None
    | Some _, Some _ ->
      epr "[ERROR] can only specify the a feature by its name or its id, not both\n%!";
      exit 1
    | Some s, None ->
      let fd = feature_descr_of_name s in
      Some fd
    | None, Some fd ->
      let fd = feature_descr_of_id fd in
      Some fd

let feature_descr_of_args name_opt id_opt =
  match name_opt, id_opt with
    | None, None -> None
    | Some _, Some _ ->
      epr "[ERROR] can only specify the a feature by its name or its id, not both\n%!";
      exit 1

    | Some s, None ->
      let ss = split_args s in
      let fds = List.map feature_descr_of_name ss in
      Some fds
    | None, Some s ->
      let ss = split_args s in
      let ids = List.map int_of_string ss in
      let fds = List.map feature_descr_of_id ids in
      Some fds

let learn
    dog_file_path
    y_name_opt
    y_id_opt
    max_depth
    initial_learning_rate
    min_convergence_rate
    num_folds
    only_fold_id
    fold_feature_name_opt
    fold_feature_id_opt
    weight_feature_name_opt
    weight_feature_id_opt
    min_observations_per_node
    convergence_rate_smoother_forgetful_factor
    deadline
    output_file_path
    selected_feature_name_regexp
    excluded_feature_name_regexp
    loss_type_s
    max_trees_opt
    max_gamma_opt
    max_features_opt
    binarize_lte
    binarize_gte
    binarize_lt
    binarize_gt
    feature_name_positive
    feature_name_negative
    feature_id_positive
    feature_id_negative
    exclude_nan_target
    exclude_inf_target
    stochastic_gradient
    best_split_algo_string
    ()
  =

  if max_depth < 1 then (
    epr "[ERROR] max-depth must be greater than 0\n%!";
    exit 1
  );

  if 0.0 >= initial_learning_rate || initial_learning_rate > 1.0 then (
    epr "[ERROR] initial-learning-rate must be in (0,1]\n%!";
    exit 1
  );

  if min_convergence_rate < 0.0 then (
    epr "[ERROR] min-convergence-rate must be non-negative\n%!";
    exit 1
  );

  if num_folds < 1 then (
    epr "[ERROR] num-folds must be positive\n%!";
    exit 1
  );
  if num_folds = 1 && max_trees_opt = None then (
    epr "[ERROR] num-folds = 1 but max number of trees not set\n%!";
    exit 1
  );

  (match only_fold_id with
    | Some i when i < 0 || i >= num_folds ->
      epr "[ERROR] invalid argument for option --only-fold: %d\n%!" i;
      exit 1
    | _ -> ()
  );

  if convergence_rate_smoother_forgetful_factor <= 0.0 ||     (* forget everything *)
     convergence_rate_smoother_forgetful_factor >= 1.0 then ( (* forget nothing *)
    epr "[ERROR] forgetful-factor must be between 0 and 1, exclusive\n%!";
    exit 1
  );

  (match max_trees_opt with
    | None -> ()
    | Some max_trees ->
      if max_trees < 0 then (
        epr "[ERROR] the maximum number of trees must be non-negative\n%!";
        exit 1
      )
  );

  (match max_gamma_opt with
    | None -> ()
    | Some max_gamma ->
      if max_gamma <= 0.0 then (
        epr "[ERROR] the maximum leaf gamma must be positive\n%!";
        exit 1
      ) else (
        epr "[INFO] max-leaf-gamma = %f" max_gamma
      )
  );

  (match max_features_opt with
    | None -> ()
    | Some max_features ->
      if max_features <= 0 then (
        epr "[ERROR] the maximum number of features must be positive\n%!";
        exit 1
      ) else (
        epr "[INFO] max-features = %d\n%!" max_features
      )
  );

  let deadline =
    match deadline with
      | Some str -> Some (deadline_of_string str)
      | None -> None (* no deadline *)
  in

  if not (Sys.file_exists dog_file_path) then (
    epr "[ERROR] file %S does not exist!\n%!" dog_file_path;
    exit 1
  );

  let output_dir_name = Filename.dirname output_file_path in
  if not (Sys.file_exists output_dir_name) then (
    epr "[ERROR] output directory %S does not exist!\n%!" output_dir_name;
    exit 1
  );

  let selected_regexp_opt =
    match selected_feature_name_regexp with
      | Some re -> (
          try
            epr "[INFO] Select feature regexp: %S\n%!" re;
            Some (Pcre.regexp re)
          with Pcre.Error _ ->
            epr "[ERROR] bad regulalar expression %S\n%!" re;
            exit 1
        )
      | None -> None
  in
  let excluded_regexp_opt =
    match excluded_feature_name_regexp with
      | Some re -> (
          try
            epr "[INFO] Exclude feature regexp: %S\n%!" re;
            Some (Pcre.regexp re)
          with Pcre.Error _ ->
            epr "[ERROR] bad regulalar expression %S\n%!" re;
            exit 1
        )
      | None -> None
  in

  let loss_type =
    match loss_type_s with
      | "logistic"-> `Logistic
      | "square" -> `Square
      | _ ->
        epr "[ERROR] bad loss type %S\n%!" loss_type_s;
        exit 1
  in

  let ys =
    match feature_descr_of_args y_name_opt y_id_opt with
      | Some x -> x
      | None ->
        epr "[ERROR] no target feature specified\n%!";
        exit 1
  in

  let fold_feature_opt =
    feature_descr_of_arg fold_feature_name_opt fold_feature_id_opt
  in

  let weight_feature_opt =
    feature_descr_of_arg weight_feature_name_opt weight_feature_id_opt
  in

  let binarization_threshold_opt =
    match binarize_lte, binarize_gte, binarize_lt, binarize_gt with
      | Some lte, None, None, None -> Some (`LTE lte)
      | None, Some gte, None, None -> Some (`GTE gte)
      | None, None, Some lt, None -> Some (`LT lt)
      | None, None, None, Some gt -> Some (`GT gt)
      | None, None, None, None -> None
      | _ ->
        epr "[ERROR] cannot specify multiple options among -gte, -lte, -gt, -lt\n%!";
        exit 1
  in

  let positive_feature_ids =
    List.fold_left (fun accu id -> IntSet.add id accu) IntSet.empty feature_id_positive
  in
  let negative_feature_ids =
    List.fold_left (fun accu id -> IntSet.add id accu) IntSet.empty feature_id_negative
  in
  let invalid_ids = IntSet.inter positive_feature_ids negative_feature_ids in
  let positive_feature_names =
    List.fold_left (fun accu name -> SSet.add name accu) SSet.empty feature_name_positive
  in
  let negative_feature_names =
    List.fold_left (fun accu name -> SSet.add name accu) SSet.empty feature_name_negative
  in
  let invalid_names = SSet.inter positive_feature_names negative_feature_names in

  let () =
    if not(IntSet.is_empty invalid_ids) || not(SSet.is_empty invalid_names) then
      begin
        prerr_endline "[ERROR] Some are features declared to be both positive and negative:";
        prerr_endline
          (String.concat ", " (List.map string_of_int (IntSet.to_list invalid_ids)));
        prerr_endline
          (String.concat ", " (SSet.to_list invalid_names));
      end
  in

  let feature_monotonicity = [] in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun id -> ((`Id id), `Positive)) (IntSet.to_list positive_feature_ids))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun id -> ((`Id id), `Negative)) (IntSet.to_list negative_feature_ids))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun name -> ((`Name name), `Positive)) (SSet.to_list positive_feature_names))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun name -> ((`Name name), `Negative)) (SSet.to_list negative_feature_names))
      feature_monotonicity in

  let best_split_algo_string = String.lowercase_ascii best_split_algo_string in
  let best_split_algo =
    match best_split_algo_string with
      | "fibonacci" -> `Fibonacci
      | "exhaustive" -> `Exhaustive
      | s ->
        epr "[ERROR] unrecognized --best-split-algo=%S\n%!" s;
        exit 1
  in
  Utils.epr "[INFO] best-split-algo=%s\n%!" best_split_algo_string;

  let conf =
    let open Sgbt in
    {
      loss_type;
      dog_file_path;
      ys;
      num_folds;
      only_fold_id;
      min_observations_per_node;
      min_convergence_rate;
      convergence_rate_smoother_forgetful_factor;
      initial_learning_rate;
      max_depth;
      deadline;
      output_file_path;
      selected_feature_name_regexp_opt = selected_regexp_opt;
      excluded_feature_name_regexp_opt = excluded_regexp_opt;
      fold_feature_opt;
      weight_feature_opt;
      max_trees_opt;
      max_gamma_opt;
      max_features_opt;
      binarization_threshold_opt;
      feature_monotonicity;
      exclude_nan_target;
      exclude_inf_target;
      stochastic_gradient;
      best_split_algo;
    }
  in

  try
    Sgbt.learn conf
  with
    | Loss.WrongTargetType y ->
      epr "[ERROR] target %s is not binary\n%!"
        (Feat_utils.string_of_feature_descr y);
      exit 1
    | Loss.BadTargetDistribution y ->
      epr "[ERROR] target %s has a bad distribution\n%!"
        (Feat_utils.string_of_feature_descr y);
      exit 1

open Cmdliner
let commands =
  let learn_cmd =
    let doc = "Learn a stochastic gradient boosting tree model. \
               The algorithm performs k-fold cross validation, training k \
               seperate sub-models which are averaged in a final model." in

    let input_file_path =
      let doc = "path of the input dog file" in
      Arg.(required & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in
    let y_name =
      let doc = "the name(s) of the response feature(s)" in
      Arg.(value & opt (some string) None &
           info ["y";"y-name";"response-name"] ~docv:"LIST" ~doc)
    in

    let y_id =
      let doc = "the id(s) of the response feature(s): if you need to provide \
                 more than one response such as for custom loss models, \
                 use a comma or space separated list" in
      Arg.(value & opt (some string) None &
           info ["Y";"y-id"; "response-id"] ~docv:"INT" ~doc)
    in

    let max_depth =
      let doc = "the maximum depth of any tree in the learned model. \
                (Tree stumps have depth=1)." in
      Arg.(value & opt int 2 & info ["d";"max-depth"] ~docv:"INT" ~doc)
    in

    let learning_rate =
      let doc = "the learning rate (aka shrinkage factor)" in
      Arg.(value & opt float 0.05 &
           info ["r";"learning-rate"] ~docv:"FLOAT" ~doc)
    in

    let min_convergence_rate =
      let doc = "learn until the convergence rate of the validation loss is \
                 below this value" in
      Arg.(value & opt float 0.001 &
           info ["c";"min-convergence-rate"] ~docv:"FLOAT" ~doc)
    in

    let num_folds =
      let doc = "the number of folds, k." in
      Arg.(value & opt int 2 & info ["k";"num-folds"] ~docv:"INT" ~doc)
    in

    let only_fold_id =
      let doc = "Train only on one fold. This option takes an \
                 integer from 0 to k-1 as an argument \
                 representing a fold id. " in
      Arg.(value & opt (some int) None & info ["only-fold"] ~docv:"INT" ~doc)
    in

    let fold_feature_name =
      let doc = "rather than assigning observations to folds randomly, \
                 use the values of the feature with this name to infer \
                 the assignment. The feature will be excluded from \
                 learning." in
      Arg.(value & opt (some string) None &
           info ["f";"fold-feature-name"] ~docv:"STRING" ~doc )
    in

    let fold_feature_id =
      let doc = "rather than assigning observations to folds randomly, \
                 use the values of the feature with this id to infer \
                 the assignment. The feature will be excluded from \
                 learning." in
      Arg.(value & opt (some int) None &
           info ["f-id";"fold-feature-id"] ~docv:"INT" ~doc )
    in

    let weight_feature_name =
      let doc = "select a real valued feature by name representing \
                 observation weights." in
      Arg.(value & opt (some string) None &
           info ["w";"weight-feature-name"] ~docv:"STRING" ~doc )
    in

    let weight_feature_id =
      let doc = "select a real valued feature by name representing \
                 observation weights." in
      Arg.(value & opt (some int) None &
           info ["w-id";"weight-feature-id"] ~docv:"INT" ~doc )
    in

    let min_observations_per_node =
      let doc = "Minimum total observation weight allowed in a tree node" in
      Arg.(value & opt float 0.0 &
           info ["W";"minimum-weight-per-node"] ~docv:"FLOAT" ~doc )
    in

    let feature_name_positive =
      let doc = "Constrain feature to have a monotonic non-decreasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all string [] &
           info ["p";"positive-feature-name"] ~docv:"STRING" ~doc )
    in
    let feature_name_negative =
      let doc = "Constrain feature to have a monotonic non-increasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all string [] &
           info ["n";"negative-feature-name"] ~docv:"STRING" ~doc )
    in
    let feature_id_positive =
      let doc = "Constrain feature to have a monotonic non-decreasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all int [] &
           info ["p-id";"positive-feature-id"] ~docv:"STRING" ~doc )
    in
    let feature_id_negative =
      let doc = "Constrain feature to have a monotonic non-increasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all int [] &
           info ["n-id";"negative-feature-id"] ~docv:"STRING" ~doc )
    in

    let convergence_rate_smoother_forgetful_factor =
      let doc = "The loss convergence rate measured over the test fold \
                 does not decrease monotonically.  A forgetful recursive \
                 least-squares model is used to smooth it, and the convergence \
                 rate test is applied to this smoothed value.  The forgetful \
                 factor, a value in (0,1) modulates the degree to which the \
                 loss of older iterations are forgotten in the smoothed convergence \
                 rate.  Values that are closer to zero are more forgetful, and \
                 therefore less smooth." in
      Arg.(value & opt float 0.9 &
           info ["m";"forgetful-factor"] ~docv:"FLOAT" ~doc)
    in

    let deadline =
      let doc = "stop learning after this much time has expired, resulting in \
          the best model learned to that point.  The deadline is expressed in \
          terms of a time offset, with \"s\" denoting seconds, \"m\" minutes, \
          \"h\" hours, and \"d\" days.  For example, \"1.2h\" represents a deadline \
          of 1.2 hours (equivalently \"72m\") from now." in
      Arg.(value & opt (some string) None &
           info ["T";"deadline"] ~docv:"DELTA-TIME" ~doc)
    in

    let output_file_path =
      let doc = "path of the output model file" in
      Arg.(required & opt (some string) None &
           info ["o";"output"] ~docv:"PATH" ~doc)
    in

    let selected_feature_name_regexp =
      let doc = "regular expression of names of features to select for \
                 learning; default is to select all features" in
      Arg.(value & opt (some string) None &
           info ["s";"select"] ~docv:"STRING" ~doc)
    in

    let excluded_feature_name_regexp =
      let doc = "regular expression of names of features to exclude from \
                 learning from among the selected features; default is to \
                 exclude no features" in
      Arg.(value & opt (some string) None &
           info ["x";"exclude"] ~docv:"STRING" ~doc)
    in

    let loss_type =
      let doc = "the kind of model to learn: either \"logistic\" for binary \
                 classifier, or \"square\" for regression" in
      Arg.(value & opt string "logistic" &
           info ["l";"loss"] ~docv:"STRING" ~doc)
    in

    let max_leaf_gamma =
      let doc = "the maximum absolute value of gamma allowed in a leaf node, \
                 prior to rescaling it by the learning rate" in
      Arg.( value & opt (some float) None &
            info ["g";"max-leaf-gamma"] ~docv:"FLOAT" ~doc)
    in

    let max_features =
      let doc = "the approximate maximum number of features in the model" in
      Arg.( value & opt (some int) None &
            info ["F";"max-features"] ~docv:"INT" ~doc)
    in

    let max_trees =
      let doc = "the maximum number of trees per fold sub-model" in
      Arg.( value & opt (some int) None &
            info ["t";"max-trees"] ~docv:"INT" ~doc)
    in

    let binarize_lte =
      let doc = "provide a binarization threshold to an ordinal prediction \
                 target.  The generated labels are \"LTE\" and \"GT\"" in
      Arg.( value & opt (some float) None &
            info ["L"; "lte"; "binarization-threshold-lte"] ~docv:"FLOAT" ~doc)
    in

    let binarize_gte =
      let doc = "provide a binarization threshold to an ordinal prediction \
                 target.  The generated labels are \"GTE\" and \"LT\"" in
      Arg.( value & opt (some float) None &
            info ["G"; "gte"; "binarization-threshold-gte"] ~docv:"FLOAT" ~doc)
    in

    let binarize_lt =
      let doc = "provide a binarization threshold to an ordinal prediction \
                 target.  The generated labels are \"LT\" and \"GTE\"" in
      Arg.( value & opt (some float) None &
            info ["lt"; "binarization-threshold-lt"] ~docv:"FLOAT" ~doc)
    in

    let binarize_gt =
      let doc = "provide a binarization threshold to an ordinal prediction \
                 target.  The generated labels are \"GT\" and \"LTE\"" in
      Arg.( value & opt (some float) None &
            info ["gt"; "binarization-threshold-gt"] ~docv:"FLOAT" ~doc)
    in

    let exclude_nan_target =
      let doc = "Exclude row where the target is a floating point nan" in
      Arg.(value & opt bool false & info ["exclude-nan-target"] ~docv:"BOOL" ~doc)
    in
    let exclude_inf_target =
      let doc = "Exclude row where the target is a floating point infinity" in
      Arg.(value & opt bool false & info ["exclude-inf-target"] ~docv:"BOOL" ~doc)
    in
    let stochastic_gradient =
      let doc = "With stochastic gradient enabled, the gradient of the loss \
                 function is computed over a random subsampling of the training \
                 set. Disabling the stochastic gradient feature allows all the \
                 training set to be used at each iteration." in
      Arg.(value & opt bool true & info ["stochastic-gradient"] ~docv:"BOOL" ~doc)
    in
    let best_split_algo_string =
      let doc = "Select algorithm to find best split. Options are `fibonacci' \
                 and `exhaustive'. Fibonacci search is much faster than exhaustive \
                 search, but it might converge to a local minimum. \
                 Default is `exhaustive'." in
      Arg.(value & opt string "exhaustive" & info ["best-split-algo"] ~docv:"[fibonacci|exhaustive]" ~doc)
    in

    Term.(pure learn $
            input_file_path $
            y_name $
            y_id $
            max_depth $
            learning_rate $
            min_convergence_rate $
            num_folds $
            only_fold_id $
            fold_feature_name $
            fold_feature_id $
            weight_feature_name $
            weight_feature_id $
            min_observations_per_node $
            convergence_rate_smoother_forgetful_factor $
            deadline $
            output_file_path $
            selected_feature_name_regexp $
            excluded_feature_name_regexp $
            loss_type $
            max_trees $
            max_leaf_gamma $
            max_features $
            binarize_lte $
            binarize_gte $
            binarize_lt $
            binarize_gt $
            feature_name_positive $
            feature_name_negative $
            feature_id_positive $
            feature_id_negative $
            exclude_nan_target $
            exclude_inf_target $
            stochastic_gradient $
            best_split_algo_string $
            const ()
         ),
    Term.info "learn" ~doc
  in
  [learn_cmd]
