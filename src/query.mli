(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
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
  val gen_query  : unit -> query
  val neg_query  : query -> query

  (* Needed for efficient evaluation *)
  val eval_row   : D.db_row -> query -> float

end

module type Qry = sig

  type   query
  module D : Db

  val gen_nquery : int -> query array
  val neg_nquery : query array -> query array

  val eval_elem   : D.db_row -> query       -> float
  val eval_query  : D.db     -> query       -> float
  val eval_nquery : D.db     -> query array -> float array
end

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry
       (* with module D   = O.D    and *)
       (*      type query = O.query *)
