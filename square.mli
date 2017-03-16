class splitter :
  optimize:float Fibsearch.minimizer ->
    max_gamma_opt:float option ->
    weights:float array ->
    y_feature:Feat.afeature ->
    n_rows:int ->
    num_observations:int ->
    min_observations_per_node:float ->
      Loss.splitter
