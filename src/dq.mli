(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Oracle
open Query

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
  res_qd_stats : (int * float * float);
}

val dq : exp_data -> exp_param -> exp_result
