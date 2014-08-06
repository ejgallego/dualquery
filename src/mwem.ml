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
open Mw

(* val mwem : exp_data -> exp_param -> exp_result *)
let mwem data param =
  let open Printf in
  printf "Starting!\n%!";

  let t       = param.exp_steps                    in
  let tf      = float_of_int t                     in

  let n       = float_of_int data.sd_info.db_elem  in

  let tot_eps = param.exp_eps                      in
  let em_eps  = tot_eps /. (2.0 *. tf)             in
  let lap_eps = 2.0 *. tf /. tot_eps               in

  let realdb  = data.sd_qcache                     in
  let qry     = data.sd_queries                    in
  let nqry    = Array.length qry                   in

  (* Universe size *)
  let usize   = Util.pow 2 data.sd_info.db_bin_att in

  (* Initial dist *)
  let d       = uniform_dist usize                 in
  let res     = uniform_dist usize                 in

  for i = 1 to t do
    printf "\nStep: %d\n%!" i;

    (* Multiplying by n *)
    let score idx =
      let res = abs_float (realdb.(idx) -. eval_bquery d qry.(idx)) *.n in
      (* printf "Score for query %d: %f \n%!" idx res; *)
      res in

    (* Print error *)
    (* printf "Query Error: \n"; *)
    (* Array.iteri (printf "q%2d: %f\n%!") (Array.init nqry score); *)

    let badquery  = exp_mech em_eps nqry score          in

    let qi        = eval_bquery d qry.(badquery)        in
    printf "Worst query: %d with error %f\n%!" badquery (realdb.(badquery) -. qi);

    (* We noise the real value *)
    let m  = realdb.(badquery) +. Laplace.lap_noise lap_eps /. n in

    let c_err = m -. qi                                          in
    printf "Corrected private error: %f \n%!" c_err;

    (* MW update rule *)
    let mw_update idx v =
      let up_factor =
          exp ( (ev_bquery idx qry.(badquery) ) *.
                c_err /. 2.0 )
      in
      (* printf "Update for %d (%f) with uf: %f\n" i !d.(i) up_factor; *)
      v *. up_factor
    in

    (* Arbitrary *)
    for k = 1 to 20 do
      Util.mapi_in_place mw_update d;
      d_norm_in_place d
    done;

    (* Add the sum of this one to the result *)
    Util.mapi_in_place (fun idx v -> v +. d.(idx)) res;

  done;
  (* Get the average of all*)

  Util.map_in_place (fun v -> v /. tf) res;
  res
