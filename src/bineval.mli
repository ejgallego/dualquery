(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

module Ni = Nativeint
open Binarray
open Query

val ba_eval_elem : bin_query -> ba -> int -> float

val ba_eval_query : ba -> bin_query -> float
val ev_norm_in_place : int -> float array -> unit

val ba_eval_bqueries_norm : bool -> ba -> bin_query array -> float array

val ba_eval_bqueries_norm_slow : bool -> ba -> bin_query array -> float array

val ba_generate_bool_bias :
  ba -> int -> int -> float -> unit

(* val ba_generate_row : *)
(*   ba -> int -> int -> float array -> unit *)

val ba_generate_bin_db_bias :
  int -> int -> ba

module Q = Query

val mk_ba_rbias_data : int -> int -> int -> Dq.exp_data
val mk_ba_rbias_data_slow : int -> int -> int -> Dq.exp_data
