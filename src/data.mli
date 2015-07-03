(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Query

module Dq : Dq.Dq
module Q = Dq.Q

type db_gen       = unit -> Q.D.db
type data_source  = (string * db_schema * db_gen)

type query_source = db_schema -> (string * Q.query array)

(* Data sources *)
val network   : data_source
val network_s : data_source
val adult     : data_source
val adult_red : data_source
val tictactoe : data_source
val ttt_bin   : data_source

val ran_queries : int -> query_source

(* Standard generator, loads the database and builds the query cache *)
val mk_exp_data : data_source -> query_source -> unit -> Dq.exp_data
