(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db

(* Ops for linear queries *)
module type Ops = sig

  type query
  module D : Gen

  val gen_query  : db_schema -> query
  val neg_query  : query -> query

  (* Needed for efficient evaluation *)
  val eval_row   : D.db_row -> query -> float

end

module type Qry = sig

  type   query
  module D : Gen

  val gen_nquery : int -> db_schema -> query array
  val neg_nquery : query array -> query array

  val eval_query  : D.db -> query -> float
  val eval_nquery : D.db -> query array -> float array
end

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry
       (* with module D   = O.D    and *)
       (*      type query = O.query *)
