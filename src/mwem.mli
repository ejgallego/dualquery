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

(* val mwem : exp_data -> exp_param -> exp_result *)
val mwem : exp_data -> exp_param -> db_dist -> db_dist
