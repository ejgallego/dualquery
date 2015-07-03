(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db

module type Ops = sig

  type query
  module D : Db

  (* val gen_query  : db_schema -> query *)
  val gen : unit -> query
  val neg : query -> query

  (* Needed for efficient evaluation *)
  val eval_row : D.db_row -> query -> float

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

end

(* Make a module form QueryOps *)
(* module Make (O : Ops) : Qry *)
(*        with module D   = O.D    and *)
(*             type query = O.query *)

(* Make a module form QueryOps *)
module Make (O : Ops) : Qry = struct

  module D   = O.D
  type query = O.query

  let gen_n n (* db_s *) =
    Array.init n (fun _ -> O.gen (* db_s *) ())

  let neg_n nqry =
    let neg_nqry = Array.map O.neg nqry in
    Array.append nqry neg_nqry

  let eval_row = O.eval_row

  let eval_db db qry =
    Array.fold_left (fun res row -> res +. (O.eval_row row qry)) 0.0 db

  let eval_db_n db =
    Array.map (eval_db db)

end

(* 3-way Marginals *)

(* Common Helpers *)
let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c

(* XXX: We could factorize most of this code too *)
module MarO = struct

  (* 3-way marginal queries over integer attributes *)
  type literal  = LEq of int * int | LNeq of int * int
  type marginal = literal * literal * literal
  type query    = PQuery of marginal | NQuery of marginal

  module D = IntDb

  let gen_literal () (* n db_schema *) =
    (* XXX *)
    let att  = 20 in
    let av   = 20 in
    (* let att    = Random.int n                      in *)
    (* let ai     = db_schema.(att)                   in *)
    (* let av     = Random.int (ai.att_max + 1)       in *)
    if Random.bool () then
      LEq (att, av)
    else
      LNeq (att, av)

  let gen () =
    let inner = (gen_literal (),
                 gen_literal (),
                 gen_literal ()) in
    PQuery inner

  let neg qry = match qry with
  | PQuery tm -> NQuery tm
  | NQuery tm -> PQuery tm

  let eval_lit row lit = match lit with
    | LEq  (n, e) -> row.(n) = e
    | LNeq (n, e) -> row.(n) <> e

  let eval_tm row (l1, l2, l3) =
    and3_att (eval_lit row l1)
             (eval_lit row l2)
             (eval_lit row l3)

  let eval_row row qry = match qry with
    | PQuery tm -> tof_att          (eval_tm row tm)
    | NQuery tm -> tof_att (not_att (eval_tm row tm))

end

module MarQ = Make(MarO)

module MarBO = struct

  type literal  = PVar of int | NVar of int
  type marginal = literal * literal * literal
  type query    = PQuery of marginal | NQuery of marginal

  module D = BinDb

  let generate_literal n_att =
    if Random.bool () then
      PVar (Random.int n_att)
    else
      NVar (Random.int n_att)

  let gen () (* db_s *) =
    (* XXXX *)
    (* let n_att    = Array.length db_s        in *)
    let n_att = 20 in
    let marginal = (generate_literal n_att,
                    generate_literal n_att,
                    generate_literal n_att) in
    PQuery marginal

  let neg qry = match qry with
  | PQuery tm -> NQuery tm
  | NQuery tm -> PQuery tm

  let eval_row_lit row lit = match lit with
  (* We must use the .() notation for -unsafe to work *)
  | PVar n ->          row.(n)
  | NVar n -> not_att  row.(n)

  let eval_marginal row (l1, l2, l3) =
    and3_att (eval_row_lit row l1)
             (eval_row_lit row l2)
             (eval_row_lit row l3)

  let eval_row row qry = match qry with
    | PQuery tm -> tof_att          (eval_marginal row tm)
    | NQuery tm -> tof_att (not_att (eval_marginal row tm))

end

module MarBQ = Make(MarBO)

(* Parity *)
let a = 3
