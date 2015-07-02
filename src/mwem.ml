(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Dbdist
open Query

(* Data for a simulation *)
type exp_data = {
  sd_info     : db_info;
  sd_queries  : bin_query array;
  sd_qcache   : float array;
}

(* Parameters for an actual experiment *)
type exp_param = {
  exp_eps     : float;
  exp_steps   : int;
}

type exp_res = db_dist
(* Result for an actual experiment *)
(*
type exp_result = {
  res_db       : db_dist;
  res_timeout  : int;
  res_seconds  : int;
  res_qd_stats : (int * float * float);
}
*)

open Expm

let htbl_keys t =
  let open Hashtbl in
  fold (fun k _ r -> k :: r) t []

let max_index arr =
  Util.foldi_left (fun idx r v -> if v >= arr.(r) then idx else r) 0 arr

(* val mwem : exp_data -> exp_param -> exp_result *)
let mwem data param init =
  let open Printf in

  let t       = param.exp_steps                    in
  let tf      = float_of_int t                     in

  let elem    = data.sd_info.db_elem               in
  let n       = float_of_int elem                  in

  let tot_eps = param.exp_eps                      in
  let lap_eps = (2.0 *. tf) /. (tot_eps *. n)      in

  let realdb  = data.sd_qcache                     in
  let qry     = data.sd_queries                    in
  let nqry    = Array.length qry                   in

  printf "Starting MWEM: Eps=%f, T=%d, n=%d, lap_eps=%f, Q=%d"
    tot_eps t elem lap_eps nqry;

  (* Initial dist *)
  let db_syn = init                                in

  (* We follow HLM and will cache the worst performing qry *)
  let open Hashtbl in
  (* Noisy real value of the query *)
  let qval : (int, float) t = create t in

  let noisy_max () =
    let res_syn   = eval_bqueries db_syn qry                                              in
    let true_err  = Array.mapi (fun idx res_v -> abs_float @@ realdb.(idx) -. res_v) res_syn in
    (* Do not select previously measured queries *)
    let keys      = htbl_keys qval                                                        in
    List.iter (fun idx -> true_err.(idx) <- 0.0) keys;
    let noisy_err = Array.map (fun err -> err +. Laplace.lap_noise lap_eps) true_err      in
    let bad_qry = max_index noisy_err in
    printf "Bad query is: %d with err: %f\n" bad_qry noisy_err.(bad_qry);
    bad_qry
  in

  let update_d q_idx err e_idx v =
    let up_factor = exp ( err *. (ev_bquery e_idx qry.(q_idx)) /. 2.0 )
    in v *. up_factor
  in

  let update_all () =
    let reps = 5 in
    for i = 1 to reps do
      (* Measured queries and noisy (true) value *)
      Hashtbl.iter (fun q_idx q_val ->
        let qval_curr = eval_bquery db_syn qry.(q_idx) in
        let q_error   = q_val -. qval_curr             in
        Util.mapi_in_place (update_d q_idx q_error) db_syn
      ) qval;
    done;
    d_norm_in_place db_syn
  in

  (* Steps of MWEM proper *)
  for i = 1 to t do
    printf "\nStep: %d\n%!" i;
    let bad_idx = noisy_max ()                         in
    Hashtbl.add qval bad_idx (realdb.(bad_idx) -. (Laplace.lap_noise lap_eps));
    update_all ()
  done;
  db_syn

(***********************************************************************)
(* Non-private version *)

let mw data param init =
  let open Printf in

  let t       = param.exp_steps                    in

  let n       = float_of_int data.sd_info.db_elem  in

  let realdb  = data.sd_qcache                     in
  let qry     = data.sd_queries                    in
  let nqry    = Array.length qry                   in

  (* Initial dist *)
  let d       = init                               in

  for i = 1 to t do
    printf "\nStep: %d\n%!" i;

    (* Multiplying by n *)
    let error    = Array.init nqry (fun idx ->
      abs_float (realdb.(idx) -. eval_bquery d qry.(idx)) *.n) in

    let badquery = Util.foldi_left (fun idx r _ -> if error.(idx) >= error.(r) then idx else r) 0 error  in

    let m  = realdb.(badquery)            in
    let qi = eval_bquery d qry.(badquery) in

    printf "Worst query is %d with error %f\n%!" badquery (realdb.(badquery) -. qi);
    let c_err = m -. qi                                 in

    (* MW update rule *)
    let mw_update idx v =
      let up_factor =
          exp ( (ev_bquery idx qry.(badquery) ) *.
                c_err /. 2.0 )
      in
      (* printf "Update for %d (%f) with uf: %f\n" idx d.(idx) up_factor; *)
      v *. up_factor
    in

    (* Arbitrary! *)
    let k_l = 20 in
    for k = 1 to k_l do
      Util.mapi_in_place mw_update d;
      d_norm_in_place d
    done;

  done;
  d
