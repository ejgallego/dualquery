(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH

   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Query
open Oracle

(* Parameters for an actual experiment *)
type exp_param = {
  exp_eta     : float;
  exp_steps   : int;
  exp_sample  : int;
  exp_timeout : int;
  exp_oracle  : (oracle_type * oracle);
}

module type Dq = sig

  module Q : Qry

  type exp_data = {
    sd_info     : db_info;
    sd_queries  : Q.query array;
    sd_qcache   : float array;
  }

  (* Result for an actual experiment *)
  type exp_result = {
    res_db       : Q.D.db;
    res_timeout  : int;
    res_seconds  : int;
    res_qd_stats : (int * float * float);
  }

  val dq : exp_data -> exp_param -> exp_result

end

module Make (Q: Qry) : Dq
