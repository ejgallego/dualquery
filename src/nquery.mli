(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Ndb

(* Ops for linear queries *)
module type QueryOps = sig

  type query
  type db_row

  val gen_query  : db_schema -> query
  val neg_query  : query -> query
  (* Needed for efficient evaluation *)
  val eval_row   : db_row -> query -> float
end

module type Q = sig

  type query
  type db

  val gen_nquery : int -> db_schema -> query array
  val neg_nquery : query array -> query

  val eval_query  : db -> query -> float
  val eval_nquery : db -> query array -> float
end

(* Make a module form QueryOps *)
(*
module Make (QO : QueryOps) (D : Db) : Q
  with type query = QO.query and
       type db    = D.db

 *)
(*
module Make_LOffset
            (V:Lattice_With_Isotropy.S)
            (LOffset : Offsetmap.S with type y = V.t and type widen_hint = V.widen_hint) =
struct
*)
