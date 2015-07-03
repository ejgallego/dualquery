(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db

module type Ops = sig

  type query
  module D : Db

  (* val gen_query  : db_schema -> query *)
  val gen_query  : unit -> query
  val neg_query  : query -> query

  (* Needed for efficient evaluation *)
  val eval_row   : D.db_row -> query -> float

end

module type Qry = sig

  type   query
  module D : Db

  (* val gen_nquery : int -> db_schema -> query array *)
  val gen_nquery : int -> query array
  val neg_nquery : query array -> query array

  val eval_elem   : D.db_row -> query       -> float
  val eval_query  : D.db     -> query       -> float
  val eval_nquery : D.db     -> query array -> float array

end

(* Make a module form QueryOps *)
(* module Make (O : Ops) : Qry *)
(*        with module D   = O.D    and *)
(*             type query = O.query *)

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry = struct

  module D   = O.D
  type query = O.query

  let gen_nquery n (* db_s *) =
    Array.init n (fun _ -> O.gen_query (* db_s *) ())

  let neg_nquery nqry =
    let neg_nqry = Array.map O.neg_query nqry in
    Array.append nqry neg_nqry

  let eval_elem = O.eval_row

  let eval_query db qry =
    Array.fold_left (fun res row -> res +. (O.eval_row row qry)) 0.0 db

  let eval_nquery db =
    Array.map (eval_query db)

end
