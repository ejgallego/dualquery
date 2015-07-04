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
  val gen : unit  -> query
  val neg : query -> query

  (* Needed for efficient evaluation *)
  val eval_row   : D.db_row -> query -> float

  (* Print to cplex *)
  val pp_cplex : Format.formatter -> int -> query -> unit

end

module type Qry = sig

  type   query
  module D : Db

  (* val gen_nquery : int -> db_schema -> query array *)
  val gen_n : int -> query array
  val neg_n : query array -> query array

  val eval_row  : D.db_row -> query       -> float
  val eval_db   : D.db     -> query       -> float
  val eval_db_n : D.db     -> query array -> float array

  (* Print to cplex *)
  val pp_cplex : Format.formatter -> int -> query -> unit

end

(* At some point this should work:

   module BinQry : Qry with module D = BinDb

*)

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry
       (* with module D   = O.D *)
        (* and type D.t = O.D.t *)
        (* and type query = O.query *)

(* Query types and their corresponding instantiations *)

module type BinLitOps = sig

  (* Here is the key ! *)

    type literal
    type marginal = literal * literal * literal
    type query    = PQuery of marginal | NQuery of marginal

    val gen_lit   : unit     -> literal
    val eval_lit  : BinDb.db_row -> literal -> bool
    val merge_lit : bool -> bool -> bool -> bool

    val pp_cplex : Format.formatter -> int -> query -> unit

end

module MakeLitOps (L : BinLitOps) : Ops
       (* with module D = BinDb *)
       (*  and type D.t = bool *)

(* 3-way marginals & parities *)
module MarBQ : Qry
module ParBQ : Qry 

(* Parity queries *)
(* module MarQ  : Qry *)
(* module ParQ  : Qry *)

