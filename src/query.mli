(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Ops for linear queries *)
open Db

module type Ops = sig

  type query
  module D : Db

  (* XXX: Db schema needed *)
  val gen : int  -> query
  val neg : query -> query

  (* Needed for efficient evaluation *)
  val eval_row   : D.db_row -> query -> float

  (* Print to cplex *)
  val pp_cplex : Format.formatter -> int -> query -> unit

  (* Print to binary variables *)
  val pp_bin_vars : Format.formatter -> int -> query -> unit

  (* Print to binary int variables *)
  val pp_int_vars : Format.formatter -> int -> query -> unit
end

module type Qry = sig

  type   query
  module D : Db

  (* val gen_nquery : int -> db_schema -> query array *)
  val gen_n : int -> int -> query array
  val neg_n : query array -> query array

  val eval_row  : D.db_row -> query       -> float
  val eval_db   : D.db     -> query       -> float
  val eval_db_n : D.db     -> query array -> float array

  (* Print to cplex *)
  val pp_cplex : Format.formatter -> int -> query -> unit

  (* Print to binary variables *)
  val pp_bin_vars : Format.formatter -> int -> query -> unit

  (* Print to binary int variables *)
  val pp_int_vars : Format.formatter -> int -> query -> unit
end

(* At some point this should work:

   module BinQry : Qry with module D = BinDb

*)

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry
       (* with type query = O.query *)
       (* with module D   = O.D *)

(* Query types and their corresponding instantiations *)

module type BinLitOps = sig

  (* Here is the key ! *)

    type literal
    type marginal = literal * literal * literal
    type query    = PQuery of marginal | NQuery of marginal

    val gen_lit   : int     -> literal
    val eval_lit  : BinDb.db_row -> literal -> bool
    val merge_lit : bool -> bool -> bool -> bool

    val pp_cplex : Format.formatter -> int -> query -> unit

    (* Print to binary variables *)
    val pp_bin_vars : Format.formatter -> int -> query -> unit

    (* Print to binary int variables *)
    val pp_int_vars : Format.formatter -> int -> query -> unit
end

module MakeLitOps (L : BinLitOps) : Ops
       (* with type query = L.query *)
       (*  and module D = BinDb *)

(* 3-way marginals & parities *)
module MarBQ : Qry
module ParBQ : Qry

(* Parity queries *)
(* module MarQ  : Qry *)
(* module ParQ  : Qry *)

