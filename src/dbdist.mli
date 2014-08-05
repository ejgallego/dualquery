(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Query

(* A database represented as a histogram  *)
type db_dist = float array

(* Convert a database to its histogram *)
val to_dist : db_info -> bin_db -> db_dist

(* [Normalized] results for the evalution of queries *)
val eval_bquery   : db_dist -> bin_query       -> float
val eval_bqueries : db_dist -> bin_query array -> float array

val print_db : out_channel -> db_dist -> unit
