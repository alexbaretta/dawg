exception WrongTargetType of Feat_utils.feature_descr
exception BadTargetDistribution of Feat_utils.feature_descr

type metrics = {
  (* string rendering of working-fold loss metrics *)
  s_wrk : string;

  (* string rendering of validation-fold loss metrics *)
  s_val : string;

  (* on the basis of validation-fold metrics, should we stop
     learning? *)
  has_converged : bool;

  (* what is the validation loss? *)
  val_loss : float
}

class type splitter = object
  method clear : unit
  method best_split : Dog_t.monotonicity -> Feat.afeature -> Proto_t.loss_split_opt
  method boost : float array -> [ `NaN of int | `Ok ]
  method update_with_subset : bool array -> unit
  method mean_model : bool array -> float
  method metrics : in_set:bool array -> out_set:bool array -> metrics
  method num_observations : int
  method write_model : Model_t.c_folds -> Model_t.feature list -> Bi_outbuf.t ->
    unit
end
