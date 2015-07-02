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

  type db
  type query

  val gen_nquery : int -> db_schema -> query array
  val neg_nquery : query array -> query array

  val eval_query  : db -> query -> float
  val eval_nquery : db -> query array -> float
end

(* Make a module form QueryOps *)
(*
module Make (QO : QueryOps) (D : Db) = struct

  type query = QO.query
  type db    = D.db

  let gen_nquery n db_s =
    Array.init n (fun _ -> QO.gen_query db_s)

  let neg_nquery nqry =
    let neg_nqry = Array.map QO.neg_query nqry in
    Array.append nqry neg_nqry

  let eval_query db qry =
    D.fold_rows (fun res row -> res +. (QO.eval_row row qry)) 0.0 db

  let eval_nquery db =
    Array.map (eval_query db)

end
 *)
(*
module Make_LOffset
            (V:Lattice_With_Isotropy.S)
            (LOffset : Offsetmap.S with type y = V.t and type widen_hint = V.widen_hint) =
struct
*)
