(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Dq

(* Information about the error in a dq run *)
type exp_error = {
  err_avg     : float;
  err_max     : float;
  err_rel_avg : float;
}

(* Result vector *)
(* Disable for now *)
(* val analyze_queries : float array -> unit *)

(* Number of queries, real_db, syn_db *)
val analyze_error : Log.ctx -> int -> float array -> float array -> exp_error

val average_exp   : (exp_data * exp_param * exp_result * exp_error) array ->
                    (exp_data * exp_param * exp_result * exp_error)
