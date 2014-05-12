(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Oracle
open Query
open Log

val init_cplex : ctx -> unit
val run_cplex  : bin_query array -> int -> oracle -> bin_db_row
