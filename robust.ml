open Proto_t

module Array = Utils.Array

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

module Aggregate = struct
  type t = {
    sum_n : float array;
    sum_z : float array;
    sum_l : float array;
  }

  let update t ~value ~n ~z ~l =
    t.sum_n.(value) <- t.sum_n.(value) +. n;
    t.sum_z.(value) <- t.sum_z.(value) +. n *. z;
    t.sum_l.(value) <- t.sum_l.(value) +. n *. l

  let create cardinality = {
    sum_n = Array.make cardinality 0.0;
    sum_z = Array.make cardinality 0.0;
    sum_l = Array.make cardinality 0.0;
  }

end

(* what would the sum_l be after the split is applied? *)
let updated_loss ~gamma  ~sum_l ~sum_z ~sum_n =
  sum_l +. sum_n *. gamma *. gamma -. 2.0 *. gamma *. sum_z


exception EmptyFold of string

class splitter
  ~optimize
  ~max_gamma_opt
  ~weights
  ~y_feature
  ~n_rows
  ~num_observations
  ~min_observations_per_node
  : Loss.splitter
  =
  let y_ord = match y_feature with
    | `Ord ord_feature ->  ord_feature
    | `Cat cat -> raise (Loss.WrongTargetType (Feat_utils.descr_of_cat_feature cat))
  in
  let { Dog_t.o_cardinality = y_cardinality; o_breakpoints = y_breakpoints } = y_ord in
  let y = Feat_utils.repr_array_of_ord_feature n_rows y_ord in

  let z = Array.make n_rows 0.0 in
  let l = Array.make n_rows 0.0 in
  let f = Array.make n_rows 0.0 in

  let n1 = n_rows + 1 in

  let cum_z = Array.make n1 0.0 in
  let cum_l = Array.make n1 0.0 in
  let cum_n = Array.make n1 0.0 in

  let in_subset = ref [| |] in

  let agg_of_vector cardinality vector =
    let in_subset_ = !in_subset in
    match vector with
    | `RLE v ->
      (* Utils.epr "[DEBUG] agg_of_vector (RLE)\n%!"; *)
      let agg = Aggregate.create cardinality in
      Rlevec.iter v (
        fun ~index ~length ~value ->
          for i = index to index + length - 1 do
            if in_subset_.(i) then
              Aggregate.update agg ~value ~n:weights.(i) ~l:l.(i) ~z:z.(i)
          done
      );
      agg

    | `Dense v ->
      (* Utils.epr "[DEBUG] agg_of_vector (Dns)\n%!"; *)
      let agg = Aggregate.create cardinality in
      let width_num_bytes = Utils.num_bytes cardinality in
      Dense.iter ~width:width_num_bytes v (
        fun ~index ~value ->
          if in_subset_.(index) then
            Aggregate.update agg ~value ~n:weights.(index) ~l:l.(index) ~z:z.(index)
      );
      agg

  in

  object
    method num_observations : int = num_observations

    method clear =
      Array.fill_all z 0.0;
      Array.fill_all l 0.0;
      Array.fill_all f 0.0;
      Array.fill_all cum_z 0.0;
      Array.fill_all cum_l 0.0;
      Array.fill_all cum_n 0.0;
      in_subset := [| |]

    (* update [f] and [zwl] based on [gamma] *)
    method boost gamma : [ `NaN of int | `Ok ] =
      let last_nan = ref None in
      Array.iteri (
        fun i gamma_i ->
          let wi = weights.(i) in
          if classify_float wi <> FP_zero then (
            (* update [f.(i)] *)
            f.(i) <- f.(i) +. gamma_i;

            let zi = y.(i) -. f.(i) in (* least-squares pseudo response *)
            let li = zi *. zi in

            (match classify_float zi with
              | FP_normal | FP_subnormal | FP_zero -> ()
              | _ -> last_nan := Some i
            );

            z.(i) <- zi;
            l.(i) <- li;
          )
      ) gamma;
      match !last_nan with
        | Some i -> `NaN i
        | None -> `Ok

    method update_with_subset in_subset_ =
      (* Utils.epr "[DEBUG] update_with_subset\n%!"; *)
      in_subset := in_subset_;
      cum_z.(0) <- 0.0;
      cum_l.(0) <- 0.0;
      cum_n.(0) <- 0.0;
      for i = 1 to n_rows do
        let i1 = i - 1 in
        if in_subset_.(i1) then (
          cum_z.(i) <- z.(i1) +. cum_z.(i1);
          cum_l.(i) <- l.(i1) +. cum_l.(i1);
          cum_n.(i) <- weights.(i1) +. cum_n.(i1)
        )
        else (
          cum_z.(i) <- cum_z.(i1);
          cum_l.(i) <- cum_l.(i1);
          cum_n.(i) <- cum_n.(i1)
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
      let agg_sum_z = agg.sum_z in
      let agg_sum_l = agg.sum_l in
      let left_sum_n = left.sum_n in
      let left_sum_z = left.sum_z in
      let left_sum_l = left.sum_l in
      let right_sum_n = right.sum_n in
      let right_sum_z = right.sum_z in
      let right_sum_l = right.sum_l in

      match kind with
        | `Cat ->

          (* categorical feature: find the partition resulting in the
             minimum loss. *)

          (* sort the levels by sum_z/n -- which is the average of the
             pseudo response's *)
          let pseudo_response_sorted =
            Array.init cardinality (
              fun k ->
                let n = agg.sum_n.(k) in
                let average_response = agg.sum_z.(k) /. n in
                k, average_response
            )
          in
          (* now, [pseudo_respones_sorted] is not really sorted yet.
               this sorts it in place: *)
          Array.sort (
            fun (_,avg_z1) (_,avg_z2) ->
              Pervasives.compare avg_z1 avg_z2
          ) pseudo_response_sorted;
          (* phew:  now [pseudo_respone_sorted] is really sorted *)

          (* [s] is index into the array of
             [pseudo_response_sorted] *)
          let s_to_k = Array.init cardinality (
              fun s ->
                let k, _ = pseudo_response_sorted.(s) in
                k
            ) in

          let k_0    = s_to_k.(0) in
          let k_last = s_to_k.(cardinality-1) in

          (* initialize the cumulative sums from left to right *)
          left_sum_n.(k_0) <- agg_sum_n.(k_0);
          left_sum_z.(k_0) <- agg_sum_z.(k_0);
          left_sum_l.(k_0) <- agg_sum_l.(k_0);

          right_sum_n.(k_last) <- agg_sum_n.(k_last);
          right_sum_z.(k_last) <- agg_sum_z.(k_last);
          right_sum_l.(k_last) <- agg_sum_l.(k_last);

          (* compute the cumulative sums from left to right *)
          for ls = 1 to cardinality-1 do

            let lk   = s_to_k.(ls)   in
            let lk_1 = s_to_k.(ls-1) in

            left_sum_n.(lk) <- left_sum_n.(lk_1) +. agg_sum_n.(lk);
            left_sum_z.(lk) <- left_sum_z.(lk_1) +. agg_sum_z.(lk);
            left_sum_l.(lk) <- left_sum_l.(lk_1) +. agg_sum_l.(lk);

            let rs = cardinality - ls - 1 in
            let rk   = s_to_k.(rs)   in
            let rk_1 = s_to_k.(rs+1) in

            right_sum_n.(rk) <- right_sum_n.(rk_1) +. agg_sum_n.(rk);
            right_sum_z.(rk) <- right_sum_z.(rk_1) +. agg_sum_z.(rk);
            right_sum_l.(rk) <- right_sum_l.(rk_1) +. agg_sum_l.(rk);

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

              let left_gamma  = left_sum_z.(k)    /. left_n  in
              let right_gamma = right_sum_z.(k_1) /. right_n in

              let left_gamma, right_gamma =
                Feat_utils.apply_max_gamma_opt ~max_gamma_opt left_gamma right_gamma
              in

              if match monotonicity with
                 | `Positive -> right_gamma > left_gamma
                 | `Negative -> right_gamma < left_gamma
                 | `Arbitrary -> true
              then (

                let loss_left = updated_loss
                  ~gamma:left_gamma
                  ~sum_l:left_sum_l.(k)
                  ~sum_z:left_sum_z.(k)
                  ~sum_n:left_n
                in

                let loss_right = updated_loss
                  ~gamma:right_gamma
                  ~sum_l:right_sum_l.(k_1)
                  ~sum_z:right_sum_z.(k_1)
                  ~sum_n:right_n
                in

                let total_loss = loss_left +. loss_right in

                let is_total_loss_smaller =
                  match !best_split with
                    | None -> true
                    | Some (best_total_loss, best_split) ->
                     total_loss < best_total_loss
                in

                if is_total_loss_smaller then (
                  let left = {
                    s_n = left_n ;
                    s_gamma = left_gamma ;
                    s_loss = loss_left;
                  }
                  in

                  let right = {
                    s_n = right_n ;
                    s_gamma = right_gamma ;
                    s_loss = loss_right;
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
            else infinity
          )
          (* done; *)
          in !best_split

        | `Ord ->

          let _ : float = Utils.Array.float_cumsum_left agg_sum_n left_sum_n in
          let _ : float = Utils.Array.float_cumsum_left agg_sum_z left_sum_z in
          let _ : float = Utils.Array.float_cumsum_left agg_sum_l left_sum_l in

          let _ : float = Utils.Array.float_cumsum_right agg_sum_n right_sum_n in
          let _ : float = Utils.Array.float_cumsum_right agg_sum_z right_sum_z in
          let _ : float = Utils.Array.float_cumsum_right agg_sum_l right_sum_l in

          let best_split = ref None in

          (* find and keep optimal split -- the one associated with the minimum loss *)
          (* for k = 0 to cardinality-2 do *)
          let _ = optimize 0 (cardinality - 2) (fun k ->
            let left_n  = left_sum_n.(k)    in
            let right_n = right_sum_n.(k+1) in

            (* we can only have a split when the left and right
               approximations are based on one or more observations *)

            if left_n > min_observations_per_node && right_n > min_observations_per_node then (

              let left_gamma  = left_sum_z.(k)    /. left_n  in
              let right_gamma = right_sum_z.(k+1) /. right_n in

              let left_gamma, right_gamma =
                Feat_utils.apply_max_gamma_opt ~max_gamma_opt left_gamma right_gamma
              in

              let loss_left = updated_loss
                  ~gamma:left_gamma
                  ~sum_l:left_sum_l.(k)
                  ~sum_z:left_sum_z.(k)
                  ~sum_n:left_n
              in

              let loss_right = updated_loss
                  ~gamma:right_gamma
                  ~sum_l:right_sum_l.(k+1)
                  ~sum_z:right_sum_z.(k+1)
                  ~sum_n:right_n
              in

              let total_loss = loss_left +. loss_right in

              let is_total_loss_smaller =
                match !best_split with
                  | None -> true
                  | Some (best_total_loss, best_split) ->
                    total_loss < best_total_loss
              in

              if is_total_loss_smaller then (
                let left = {
                  s_n = left_n ;
                  s_gamma = left_gamma ;
                  s_loss = loss_left;
                }
                in

                let right = {
                  s_n = right_n ;
                  s_gamma = right_gamma ;
                  s_loss = loss_right;
                }
                in

                let curr_split = `OrdinalSplit {
                    os_feature_id = feature_id;
                    os_split = k ;
                    os_left = left ;
                    os_right = right ;
                  }
                in
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
          Utils.add_to wrk_loss (wi *. l.(i));
          (* Utils.pr "[DEBUG] in_set i=%d w=%f l=%f z=%f wrk_nn=%f wrk_loss=%f\n%!" *)
          (*   i weights.(i) l.(i) z.(i) !wrk_nn !wrk_loss *)
        );

        if out_set.(i) then (
          let wi = weights.(i) in
          Utils.add_to val_nn wi;
          Utils.add_to val_loss (wi *. l.(i));
          (* Utils.pr "[DEBUG] out_set i=%d w=%f l=%f z=%f val_nn=%f val_loss=%f\n%!" *)
          (*   i weights.(i) l.(i) z.(i) !val_nn !val_loss *)
        )
      done;

      if !wrk_nn > 0.0 && !val_nn > 0.0 then
        let wrk_loss = !wrk_loss /. !wrk_nn in

        let val_loss = !val_loss /. !val_nn in

        let s_wrk  = Printf.sprintf "% 8.2f % 6.4e" !wrk_nn wrk_loss in
        let s_val  = Printf.sprintf "% 8.2f % 6.4e" !val_nn val_loss in

        Loss.( { s_wrk; s_val; has_converged = false; val_loss; } )

      else
        raise (EmptyFold (Printf.sprintf "wrk_nn=%0.2f val_nn=%0.2f" !wrk_nn !val_nn))

    method metrics_header = Printf.sprintf "%5s % 11s" "n" "loss"

    method mean_model ~in_set ~out_set : float =
      let nn = ref 0.0 in
      let hist_array = Array.make y_cardinality 0.0 in
      Feat_utils.iter_ord_by_level (fun i level ->
        if in_set.(i) then
          let wi = weights.(i) in
          hist_array.(level) <- hist_array.(level) +. wi;
          Utils.add_to nn wi
      ) y_ord;

      let median_w = !nn /. 2.0 in
      let repr_elements = Feat_utils.repr_elements_of_ord_feature y_ord in
      let rec median k prev_cum_w =
        assert (prev_cum_w < median_w);
        let delta_w = hist_array.(k) in
        let cum_w = prev_cum_w +. delta_w in
        let excess_w = cum_w -. median_w in
        if excess_w >= 0.0 then
          let repr = repr_elements.(k) in
          if k = 0 then
            repr
          else
            let prev_repr = repr_elements.(k-1) in
            let median = prev_repr +. (repr -. prev_repr) *. (delta_w -. excess_w) /. delta_w in
            Utils.epr "[DEBUG] robust mean model: median_w=%f, k=%d, repr=%f, prev=%f, excess_w=%f, median=%f\n%!"
              median_w k repr prev_repr excess_w median;
            median
        else
          median (succ k) cum_w
      in
      let gamma0 = median 0 0.0 in
      gamma0

    method write_model re_folds re_features out_buf =
      let open Model_t in
      let re_num_folds = List.length re_folds in
      let model = `Square { re_num_folds; re_folds; re_features } in
      Model_j.write_c_model out_buf model

  end
