open Proto_t

module Array = Utils.Array

external int_of_bool : bool -> int = "%identity"
external identity : 'a -> 'a = "%identity"

let pr = Utils.pr
let epr = Utils.epr

type metrics = {
  n : float;
  loss : float;
}

let loss { loss } =
  loss, false (* unlike logistic, square objective
                 has only one metric -- the loss itself *)

type model = Model_t.l_regression_model

let string_of_metrics { n; loss } =
  Printf.sprintf "% 6.2f %.4e" n loss

let y_repr_array y_feature n =
  let open Dog_t in
  match y_feature with
    | `Ord ord_feature -> Feat_utils.repr_array_of_ord_feature n ord_feature
    | `Cat cat -> raise (Loss.WrongTargetType (Feat_utils.descr_of_cat_feature cat))

module Aggregate = struct
  type t = {
    (* Aggregate observation weight per bin *)
    sum_n : float array;

    (* Aggregate loss per bin. This might not be necessary. *)
    sum_loss : float array;

    (* Right pseudo-response per bin: aggregate delta-loss for a positive unit increment in f *)
    sum_zplus : float array;

    (* Left pseudo-response per bin: aggregate delta-loss for a negative unit increment in f *)
    sum_zminus : float array;
  }

  let update t ~value ~n ~loss ~zplus ~zminus =
    t.sum_n.(value) <- t.sum_n.(value) +. n;
    t.sum_loss.(value)   <- t.sum_loss.(value) +. n *. loss;
    t.sum_zplus.(value)  <- t.sum_zplus.(value)  +. n *. zplus;
    t.sum_zminus.(value) <- t.sum_zminus.(value) +. n *. zminus

  let create cardinality = {
    sum_n = Array.make cardinality 0.0;
    sum_loss = Array.make cardinality 0.0;
    sum_zplus  = Array.make cardinality 0.0;
    sum_zminus = Array.make cardinality 0.0;
  }

end

(* what would the sum_l be after the split is applied? *)
let updated_loss ~gamma  ~sum_l ~sum_z ~sum_n =
  (* TODO *)
  assert false


let y_repr_table optimization y_features n_rows =
  let open Dog_t in
  let ord_features = List.map (function
    | `Ord ord_feature -> ord_feature
    | `Cat cat -> raise (Loss.WrongTargetType (Feat_utils.descr_of_cat_feature cat))
  ) y_features
  in
  let scale = match optimization with
    | `Minimize -> 1.0
    | `Maximize -> -1.0
  in
  Feat_utils.repr_table_of_ord_features ~scale n_rows ord_features

exception EmptyFold of string

class splitter
  ~optimization
  ~optimize
  ~weights
  ~y_features
  ~n_rows
  ~num_observations
  ~min_observations_per_node
  ~force_mean
  =
  let ys = y_repr_table optimization y_features n_rows in

  (* The decision function can only select from among the
     action values provided in the ys table *)
  let m = List.length y_features in
  let max_f = m - 1 in

  let () = Utils.epr "[INFO] custom model with %d levels\n%!" m in
  let () = List.iteri (fun i feature ->
    let descr = Feat_utils.string_descr_of_feature feature in
    Utils.epr "[INFO] level % 3d: %s\n%!" i descr
  ) y_features in

  (* current index into the loss row vector *)
  let f = Array.make n_rows 0 in

  (* row-wise value of the loss function computed on f *)
  let loss = Array.make n_rows 0.0 in

  (* row-wise value of the right pseudo-response: delta-loss for positive unit increment of *)
  let zplus  = Array.make n_rows 0.0 in

  (* row-wise value of the left pseudo-response: delta-loss for negative unit increment of *)
  let zminus = Array.make n_rows 0.0 in

  let n1 = n_rows + 1 in

  (* cumulative observation weight *)
  let cum_n = Array.make n1 0.0 in

  (* cumulative value of the loss function computed on f *)
  let cum_loss = Array.make n1 0.0 in

  (* cumulative value of the right pseudo-response *)
  let cum_zplus  = Array.make n1 0.0 in

  (* cumulative value of the left pseudo-response *)
  let cum_zminus = Array.make n1 0.0 in

  let in_subset = ref [| |] in

  let agg_of_vector cardinality vector =
    let in_subset_ = !in_subset in
    match vector with
      | `RLE v ->
        (* Utils.epr "[DEBUG] agg_of_vector (RLE)\n%!"; *)
        let agg = Aggregate.create cardinality in
        Rlevec.iter v (
          fun ~index ~length ~value ->
            (* ~value is the integer id of the bin *)
            (* the value is constant for ~length observations starting at row ~index *)
            for i = index to index + length - 1 do
              if in_subset_.(i) then
                Aggregate.update agg ~value ~n:weights.(i)
                  ~loss:loss.(i) ~zplus:zplus.(i) ~zminus:zminus.(i)
            done
        );
        agg

      | `Dense v ->
        (* Utils.epr "[DEBUG] agg_of_vector (Dns)\n%!"; *)
        let agg = Aggregate.create cardinality in
        let width_num_bytes = Utils.num_bytes cardinality in
        Dense.iter ~width:width_num_bytes v (
          fun ~index ~value ->
            (* ~value is the integer id of the bin *)
            (* ~index is the row number *)
            if in_subset_.(index) then
              Aggregate.update agg ~value ~n:weights.(index)
                ~loss:loss.(index) ~zplus:zplus.(index) ~zminus:zminus.(index)
        );
        agg
  in

  let apply_boost f gamma = max 0 (min max_f (f + gamma)) in

  object
    val mutable mean_model_loss = (nan, nan)
    method num_observations : int = num_observations

    method clear =
      Array.fill_all f 0;
      Array.fill_all loss 0.0;
      Array.fill_all zplus 0.0;
      Array.fill_all zminus 0.0;
      Array.fill_all cum_loss 0.0;
      Array.fill_all cum_zplus 0.0;
      Array.fill_all cum_zminus 0.0;
      Array.fill_all cum_n 0.0;
      in_subset := [| |]

    (* update [f] and [zwl] based on [gamma] *)
    (* We are no longer checking for NaNs here, so we have to do it later *)
    method boost gamma : [ `NaN of int | `Ok ] =
      let savings = ref 0.0 in
      Array.iteri (
        fun i gamma_if ->
          let gamma_i = int_of_float gamma_if in
          let wi = weights.(i) in
          if classify_float wi <> FP_zero then (
            (* update [f.(i)] *)
            let f_i = apply_boost f.(i) gamma_i in
            let f_plus_i = apply_boost f_i 1 in
            let f_minus_i = apply_boost f_i (-1) in
            let loss_i = ys.(i).(f_i) in (* custom loss function for the i-th row for a decision value f_i *)
            let loss_plus_i = ys.(i).(f_plus_i) in
            let loss_minus_i = ys.(i).(f_minus_i) in
            let zplus_i = loss_plus_i -. loss_i in (* right pseudo response for the i-th row *)
            let zminus_i = loss_minus_i -. loss_i in (* left pseudo response for the i-th row *)
            savings := !savings +. loss.(i) -. loss_i;
            (* if i mod 10_000 = 0 && gamma_i <> 0 then *)
            (*   Utils.epr "[DEBUG] i=%d gamma=%.2f->%d f=%d->%d loss=%.2f->%.2f savings=%.2f\n%!" *)
            (*     i gamma_if gamma_i f.(i) f_i loss.(i) loss_i !savings; *)
            f.(i) <- f_i;
            zplus.(i) <- zplus_i;
            zminus.(i) <- zminus_i;
            loss.(i) <- loss_i;
          )
      ) gamma;
      `Ok

    method update_with_subset in_subset_ =
      in_subset := in_subset_;
      cum_loss.(0) <- 0.0;
      cum_zplus.(0) <- 0.0;
      cum_zminus.(0) <- 0.0;
      cum_n.(0) <- 0.0;
      for i = 1 to n_rows do
        let i1 = i - 1 in
        if in_subset_.(i1) then (
          cum_loss.(i)   <- loss.(i1) +. cum_loss.(i1);
          cum_zplus.(i)  <- zplus.(i1) +. cum_zplus.(i1);
          cum_zminus.(i) <- zminus.(i1) +. cum_zminus.(i1);
          cum_n.(i)      <- weights.(i1) +. cum_n.(i1)
        )
        else (
          cum_loss.(i)   <- cum_loss.(i1);
          cum_zplus.(i)  <- cum_zplus.(i1);
          cum_zminus.(i) <- cum_zminus.(i1);
          cum_n.(i)      <- cum_n.(i1)
        )
      done

    method best_split
             (monotonicity : Dog_t.monotonicity)
             feature
           : (float * Proto_t.split) option
      =
      let feature_id = Feat_utils.id_of_feature feature in

      let open Aggregate in
      let open Dog_t in
      let cardinality, kind, agg =
        match feature with
          | `Ord { o_cardinality; o_vector; o_feature_id } ->
            let agg = agg_of_vector o_cardinality o_vector in
            o_cardinality, `Ord, agg

          | `Cat { c_cardinality; c_vector; c_feature_id; c_feature_name_opt } ->
            let agg = agg_of_vector c_cardinality c_vector in
            if monotonicity <> `Arbitrary then
              Printf.ksprintf failwith
                "monotonic marginal effect not supported for categorical feature %d%s"
                c_feature_id (match c_feature_name_opt with
                                | Some(s) -> Printf.sprintf " (%s)" s
                                | None -> "")
            else
              c_cardinality, `Cat, agg
      in
      if cardinality - 2 < 0 then None else

      (* let last = cardinality - 1 in *)
      let left = Aggregate.create cardinality in
      let right = Aggregate.create cardinality in
      let agg_sum_n = agg.sum_n in
      let agg_sum_zplus = agg.sum_zplus in
      let agg_sum_zminus = agg.sum_zminus in
      let agg_sum_loss = agg.sum_loss in
      let left_sum_n = left.sum_n in
      let left_sum_zplus = left.sum_zplus in
      let left_sum_zminus = left.sum_zminus in
      let left_sum_loss = left.sum_loss in
      let right_sum_n = right.sum_n in
      let right_sum_zplus = right.sum_zplus in
      let right_sum_zminus = right.sum_zminus in
      let right_sum_loss = right.sum_loss in


      match kind with
        | `Cat ->
          (* categorical feature: find the partition resulting in the
             minimum loss. *)

          (* sort the levels by sum_z/n -- which is the average of the
             pseudo response's *)
          let pseudo_response_sorted =
            Array.init cardinality (
              fun k ->
                let n = agg_sum_n.(k) in
                let average_response = (min agg_sum_zplus.(k) agg_sum_zplus.(k)) /. n in
                k, average_response
            )
          in
          (* now, [pseudo_respones_sorted] is not really sorted yet.
               this sorts it in place: *)
          let cmp_z (_,avg_z1) (_,avg_z2) = Pervasives.compare avg_z1 avg_z2 in
          Array.fast_sort cmp_z pseudo_response_sorted;
          (* phew:  now [pseudo_respone_sorted] is really sorted *)

          (* [s] is index into the array of
             [pseudo_response_sorted] *)
          let s_to_k = Array.map fst pseudo_response_sorted in

          let k_0    = s_to_k.(0) in
          let k_last = s_to_k.(cardinality-1) in

          (* initialize the cumulative sums from left to right *)
          left_sum_n.(k_0)      <- agg_sum_n.(k_0);
          left_sum_zplus.(k_0)  <- agg_sum_zplus.(k_0);
          left_sum_zminus.(k_0) <- agg_sum_zminus.(k_0);
          left_sum_loss.(k_0)   <- agg_sum_loss.(k_0);

          right_sum_n.(k_last)      <- agg_sum_n.(k_last);
          right_sum_zplus.(k_last)  <- agg_sum_zplus.(k_last);
          right_sum_zminus.(k_last) <- agg_sum_zminus.(k_last);
          right_sum_loss.(k_last)   <- agg_sum_loss.(k_last);

          (* compute the cumulative sums from left to right *)
          for ls = 1 to cardinality-1 do

            let lk   = s_to_k.(ls)   in
            let lk_1 = s_to_k.(ls-1) in

            left_sum_n.(lk)      <- left_sum_n.(lk_1) +. agg_sum_n.(lk);
            left_sum_zplus.(lk)  <- left_sum_zplus.(lk_1) +. agg_sum_zplus.(lk);
            left_sum_zminus.(lk) <- left_sum_zminus.(lk_1) +. agg_sum_zminus.(lk);
            left_sum_loss.(lk)   <- left_sum_loss.(lk_1) +. agg_sum_loss.(lk);

            let rs = cardinality - ls - 1 in
            let rk   = s_to_k.(rs)   in
            let rk_1 = s_to_k.(rs+1) in

            right_sum_n.(rk)      <- right_sum_n.(rk_1) +. agg_sum_n.(rk);
            right_sum_zplus.(rk)  <- right_sum_zplus.(rk_1) +. agg_sum_zplus.(rk);
            right_sum_zminus.(rk) <- right_sum_zminus.(rk_1) +. agg_sum_zminus.(rk);
            right_sum_loss.(rk)   <- right_sum_loss.(rk_1) +. agg_sum_loss.(rk);

          done;

          let best_split = ref None in

          (* find and keep optimal split -- the one associated with the
             minimum loss *)
          (* for s = 0 to cardinality-2 do *)
          let _ = optimize 0 (cardinality - 2) (fun s ->
            let k   = s_to_k.(s)   in
            let k_1 = s_to_k.(s+1) in

            let left_n  = left_sum_n.(k)    in
            let right_n = right_sum_n.(k_1) in

            (* we can only have a split when the left and right
               approximations are based on one or more observations *)

            if left_n > min_observations_per_node && right_n > min_observations_per_node then (

              let left_zplus = left_sum_zplus.(k) in
              let left_zminus = left_sum_zminus.(k) in
              let left_loss = left_sum_loss.(k) in
              let left_gamma, left_z =
                match compare left_zplus left_zminus with
                  | -1 -> if left_zplus < 0.0 then 1, left_zplus else 0, 0.0
                  | 1  -> if left_zminus < 0.0 then -1, left_zminus else 0, 0.0
                  | 0  -> 0, 0.0
                  | _ -> assert false
              in

              let right_zplus = right_sum_zplus.(k) in
              let right_zminus = right_sum_zminus.(k) in
              let right_loss = right_sum_loss.(k) in
              let right_gamma, right_z =
                match compare right_zplus right_zminus with
                  | -1 -> if right_zplus < 0.0 then 1, right_zplus else 0, 0.0
                  | 1  -> if right_zminus < 0.0 then -1, right_zminus else 0, 0.0
                  | 0  -> 0, 0.0
                  | _ -> assert false
              in

              let total_loss = left_loss +. left_z +. right_loss +. right_z in
              (* Utils.epr "[DEBUG] searching: id=%d k=%d/%d total_loss=%f left_zplus=%f left_zminus=%f left_z=%f right_zplus=%f right_zminus=%f right_z=%f\n%!" *)
              (*   feature_id k cardinality total_loss left_zplus left_zminus left_z right_zplus right_zminus right_z; *)
              if right_gamma = 0 && left_gamma = 0 then
                total_loss
              else
                let is_total_loss_smaller =
                  match !best_split with
                    | None -> true
                    | Some (best_total_loss, best_split) ->
                     total_loss < best_total_loss
                in

                if is_total_loss_smaller then (
                  let left = {
                    s_n = left_n;
                    s_gamma = float left_gamma;
                    s_loss = left_loss;
                  }
                  in

                  let right = {
                    s_n = right_n;
                    s_gamma = float right_gamma;
                    s_loss = right_loss;
                  }
                  in

                  let ord_split = {
                    os_feature_id = feature_id;
                    os_split = s;
                    os_left = left;
                    os_right = right;
                  } in

                  let split = `CategoricalSplit (ord_split, s_to_k) in
                  best_split := Some (total_loss, split)
                );
                total_loss
            )
            else infinity
          )
          (* done; *)
          in !best_split

        | `Ord ->

          let _ : float = Array.float_cumsum_left agg_sum_n left_sum_n in
          let _ : float = Array.float_cumsum_left agg_sum_zplus left_sum_zplus in
          let _ : float = Array.float_cumsum_left agg_sum_zminus left_sum_zminus in
          let _ : float = Array.float_cumsum_left agg_sum_loss left_sum_loss in

          let _ : float = Array.float_cumsum_right agg_sum_n right_sum_n in
          let _ : float = Array.float_cumsum_right agg_sum_zplus right_sum_zplus in
          let _ : float = Array.float_cumsum_right agg_sum_zminus right_sum_zminus in
          let _ : float = Array.float_cumsum_right agg_sum_loss right_sum_loss in

          let best_split = ref None in

          (* find and keep optimal split -- the one associated with the minimum loss *)
          (* for k = 0 to cardinality-2 do *)
          let _ = optimize 0 (cardinality - 2) (fun k ->
            let left_n  = left_sum_n.(k)    in
            let right_n = right_sum_n.(k+1) in

            (* we can only have a split when the left and right
               approximations are based on one or more observations *)

            if left_n > min_observations_per_node && right_n > min_observations_per_node then (

              let left_zplus = left_sum_zplus.(k) in
              let left_zminus = left_sum_zminus.(k) in
              let left_loss = left_sum_loss.(k) in
              let left_gamma, left_z =
                match compare left_zplus left_zminus with
                  | -1 -> if left_zplus < 0.0 then 1, left_zplus else 0, 0.0
                  | 1  -> if left_zminus < 0.0 then -1, left_zminus else 0, 0.0
                  | 0  -> 0, 0.0
                  | _ -> assert false
              in

              let right_zplus = right_sum_zplus.(k) in
              let right_zminus = right_sum_zminus.(k) in
              let right_loss = right_sum_loss.(k) in
              let right_gamma, right_z =
                match compare right_zplus right_zminus with
                  | -1 -> if right_zplus < 0.0 then 1, right_zplus else 0, 0.0
                  | 1  -> if right_zminus < 0.0 then -1, right_zminus else 0, 0.0
                  | 0  -> 0, 0.0
                  | _ -> assert false
              in

              let total_loss = left_loss +. left_z +. right_loss +. right_z in
              (* Utils.epr "[DEBUG] searching: id=%d k=%d/%d total_loss=%f left_zplus=%f left_zminus=%f left_z=%f right_zplus=%f right_zminus=%f right_z=%f\n%!" *)
              (*   feature_id k cardinality total_loss left_zplus left_zminus left_z right_zplus right_zminus right_z; *)
              if right_gamma = 0 && left_gamma = 0 then
                total_loss
              else
                let is_total_loss_smaller =
                  match !best_split with
                    | None -> true
                    | Some (best_total_loss, best_split) ->
                      total_loss < best_total_loss
                in

                if is_total_loss_smaller then (
                  let left = {
                    s_n = left_n;
                    s_gamma = float left_gamma;
                    s_loss = left_loss;
                  }
                  in

                  let right = {
                    s_n = right_n;
                    s_gamma = float right_gamma ;
                    s_loss = right_loss;
                  }
                  in

                  let curr_split = `OrdinalSplit {
                    os_feature_id = feature_id;
                    os_split = k;
                    os_left = left;
                    os_right = right;
                  }
                  in
                  (* Utils.epr "[DEBUG] best split id=%d k=%d/%d total_loss=%f left_zplus=%f left_zminus=%f left_z=%f right_zplus=%f right_zminus=%f right_z=%f\n%!" *)
                  (*   feature_id k cardinality total_loss left_zplus left_zminus left_z right_zplus right_zminus right_z; *)
                  best_split := Some (total_loss, curr_split)
                );
                total_loss
            )
            else
              infinity
          )
          (* done; *)
          in !best_split

    method metrics ~in_set ~out_set =
      let wrk_loss = ref 0.0 in
      let wrk_nn = ref 0.0 in
      let val_loss = ref 0.0 in
      let val_nn = ref 0.0 in

      for i = 0 to n_rows - 1 do
        if in_set.(i) then (
          let wi = weights.(i) in
          Utils.add_to wrk_nn wi;
          Utils.add_to wrk_loss (wi *. loss.(i));
          (* Utils.pr "[DEBUG] in_set i=%d w=%f l=%f z=%f wrk_nn=%f wrk_loss=%f\n%!" *)
          (*   i weights.(i) l.(i) z.(i) !wrk_nn !wrk_loss *)
        );

        if out_set.(i) then (
          let wi = weights.(i) in
          Utils.add_to val_nn wi;
          Utils.add_to val_loss (wi *. loss.(i));
          (* Utils.pr "[DEBUG] out_set i=%d w=%f l=%f z=%f val_nn=%f val_loss=%f\n%!" *)
          (*   i weights.(i) l.(i) z.(i) !val_nn !val_loss *)
        )
      done;

      if !wrk_nn > 0.0 && !val_nn > 0.0 then
        let wrk_loss = !wrk_loss /. !wrk_nn in
        let val_loss = !val_loss /. !val_nn in

        let (wrk_loss_mean_model, val_loss_mean_model) = mean_model_loss in
        (* let wrk_loss, val_loss, wrk_loss_mean_model, val_loss_mean_model = *)
        (*   match optimization with *)
        (*     | `Maximize -> ~-.wrk_loss, ~-.val_loss, ~-.wrk_loss_mean_model, ~-.val_loss_mean_model *)
        (*     | `Minimize -> wrk_loss, val_loss, wrk_loss_mean_model, val_loss_mean_model *)
        (* in *)

        let (~.) = match optimization with
          | `Maximize -> (~-.)
          | `Minimize -> identity
        in
        let delta_wrk = wrk_loss -. wrk_loss_mean_model in
        let delta_val = val_loss -. val_loss_mean_model in
        let delta_wrk_pct = delta_wrk /. wrk_loss *. 100.0 in
        let delta_val_pct = delta_val /. val_loss *. 100.0 in
        let s_wrk = Printf.sprintf "% 8.2f % .4e % .4e % .4e%%"
          !wrk_nn ~.wrk_loss ~.delta_wrk delta_wrk_pct
        in
        let s_val = Printf.sprintf "% 8.2f % .4e % .4e % .4e%%"
          !val_nn ~.val_loss ~.delta_val delta_val_pct
        in

        Loss.( { s_wrk; s_val; has_converged = false; val_loss; } )

      else
        raise (EmptyFold (Printf.sprintf "wrk_nn=%0.2f val_nn=%0.2f" !wrk_nn !val_nn))

    method metrics_header =
      Printf.sprintf "% 5s % 11s % 11s % 11s%%" "n" "obj" "delta" "delta"

    method mean_model ~in_set ~out_set : float =
      match force_mean with
        | Some gamma0 ->
          let (wrk_n,val_n,wrk_loss,val_loss) = Array.foldi_left (
            fun i (wrk_n,val_n,wrk_loss,val_loss) yi ->
              let wrk_n = if in_set.(i) then wrk_n +. weights.(i) else wrk_n in
              let val_n = if out_set.(i) then val_n +. weights.(i) else val_n in
              let wrk_loss = if in_set.(i) then wrk_loss +. yi.(gamma0) else wrk_loss in
              let val_loss = if out_set.(i) then val_loss +. yi.(gamma0) else val_loss in
              (wrk_n,val_n,wrk_loss, val_loss)
          ) (0.0, 0.0, 0.0, 0.0) ys
          in
          let l_i = wrk_loss /. wrk_n in
          let l_o = val_loss /. val_n in
          mean_model_loss <- (l_i, l_o);
          let gamma = float gamma0 in
          Utils.epr "[DEBUG mean_model] gamma=%d->%.2f\n%!" gamma0 gamma;
          gamma
        | None ->
          let sum_wrk_n = ref 0.0 in
          let sum_val_n = ref 0.0 in
          let sum_wrk_ys : float array = Array.make m 0.0 in
          let sum_val_ys : float array = Array.make m 0.0 in
          Array.iteri (fun i y_i ->
            if in_set.(i) then (
              sum_wrk_n := !sum_wrk_n +. weights.(i);
              Array.iteri (fun j y_ij -> sum_wrk_ys.(j) <- sum_wrk_ys.(j) +. y_ij) y_i;
            );
            if out_set.(i) then (
              sum_val_n := !sum_val_n +. weights.(i);
              Array.iteri (fun j y_ij -> sum_val_ys.(j) <- sum_val_ys.(j) +. y_ij) y_i;
            )
          ) ys;
          let gamma0, wrk_loss = optimize 0 max_f (Array.get sum_wrk_ys) in
          let l_i = wrk_loss /. !sum_wrk_n in
          let l_o = sum_val_ys.(gamma0) /. !sum_val_n in
          mean_model_loss <- (l_i, l_o);
          let gamma = float gamma0 in
          Utils.epr "[DEBUG mean_model] gamma=%d->%.2f\n%!" gamma0 gamma;
          gamma

    method write_model re_folds re_features out_buf =
      let open Model_t in
      let re_num_folds = List.length re_folds in
      let model = `Custom { re_num_folds; re_folds; re_features } in
      Model_j.write_c_model out_buf model

  end
