(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Unix
open Printf

open Db
open Oracle
open Query
open Qdist

open Cplex

(* Data for a simulation *)
type exp_data = {
  sd_info     : db_info;
  sd_queries  : bin_query array;
  sd_qcache   : float array;
}

(* Parameters for an actual experiment *)
type exp_param = {
  exp_eta     : float;
  exp_steps   : int;
  exp_sample  : int;
  exp_timeout : int;
  exp_oracle  : (oracle_type * oracle);
}

(* Result for an actual experiment *)
type exp_result = {
  res_db       : bin_db;
  res_timeout  : int;
  res_seconds  : int;
  res_qd_stats : (int * float * float)
}

(* The Main routine *)
let dq data exp =
  (* Boring initialization *)
  let atts     = data.sd_info.db_bin_att                        in
  let queries  = data.sd_queries                                in
  let nqry     = Array.length queries                           in
  let eta      = exp.exp_eta                                    in
  let sample   = exp.exp_sample                                 in
  let steps    = exp.exp_steps                                  in

  let (_, oracle) = exp.exp_oracle                              in
  let query_cache = data.sd_qcache                              in

  (* Create the query distribution *)
  let qdist    = qd_new nqry                                    in

  let timeouts = ref 0                                          in

  (* Main loop *)
  let start_time = Unix.time ()                                 in

  (* Create the result array *)
  let result = Array.init steps (fun i ->
    printf "**** Starting iteration: %d of %d [E:%2f, T:%d, S:%d]\n%!" (i + 1) steps eta steps sample;

    let s_time    = Unix.time () in
    printf "**** Sampling\n%!";

    let sarray = qd_nsample sample qdist queries                in

    printf "**** Sampling finished in: %f\n%!" ((Unix.time ()) -. s_time);

    let p_time    = Unix.time ()                                in
    let syn_elem  = run_cplex sarray atts oracle                in
    let a_time    = Unix.time ()                                in

    if (int_of_float (a_time -. p_time) + 1 >= exp.exp_timeout) then
      incr timeouts
    else ();

    printf "**** Updating query distribution\n%!";

    qd_update_in_place qdist queries query_cache syn_elem eta;

    syn_elem
  ) in
  let end_time    = Unix.time ()                                in
  {
    res_db       = result;
    res_timeout  = !timeouts;
    res_seconds  = int_of_float (end_time -. start_time);
    res_qd_stats = qd_stats qdist;
  }


