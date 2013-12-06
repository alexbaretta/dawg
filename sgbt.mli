type conf = {
  dog_file_path : string;
  num_folds : int;
  min_convergence_rate : float;
  initial_learning_rate : float;
  y : Feat_map.feature_descr;
  max_depth : int;
  convergence_rate_smoother_forgetful_factor : float;
  deadline : float option;
  output_file_path : string;
  excluded_feature_name_regexp_opt : Pcre.regexp option;
  fold_feature_opt : Feat_map.feature_descr option;
  max_trees_opt : int option;
}

module type SGBT = sig
  val learn : conf -> unit
end

module Make : functor ( L : Loss.LOSS ) -> SGBT
