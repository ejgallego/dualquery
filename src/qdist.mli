(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Distributions over queries *)

open Array

open Db
open Query

(* Array of the weights of each query *)
type qdist = {
  elem : int;
  dist : float array ; 			(* The actual distribution *)

  (* Alias sampling arrays *)
  prob : float array ;
  alias: int   array ;
}

(* New uniform distribution *)
val qd_new : int -> qdist

(* Stats of the distribution (zeros, min, max) *)
val qd_stats  : qdist -> (int * float * float)

(* Returns n queries sampled from dist *)
val qd_nsample     : int -> qdist -> 'a array -> 'a array

(* Due to efficiency reasons *)
module type QDUpdater = sig

  module Q : Qry

  (* qd_update : qdist -> queries -> query_cache -> syn_elem *)
  val qd_update_in_place : qdist -> Q.query array -> float array -> Q.D.db_row -> float -> unit

end

module Make (Q : Qry) : QDUpdater with module Q = Q

