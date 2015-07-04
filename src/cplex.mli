(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Oracle
open Query
open Log

module type Cplex = sig

  module Q : Qry

  val init_cplex : ctx -> unit
  val run_cplex  : Q.query array -> int -> oracle -> Q.D.db_row

end

module Make (Q : Qry) : Cplex with module Q = Q
