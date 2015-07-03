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

(* 3-way Marginals and Parities *)

module type LiteralOps = sig

    type literal
    module D : Db

    val gen_lit   : unit     -> literal
    val eval_lit  : D.db_row -> literal -> bool
    val merge_lit : bool -> bool -> bool -> bool

end

(* Common Helpers *)
let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c
let par3_att a b c =  (a && b && not c) ||     (c && a && not b)
                   || (b && c && not a) || not (a || b ||     c)

module MakeLitOps (L : LiteralOps) : Ops = struct

  type marginal = L.literal * L.literal * L.literal
  type query    = PQuery of marginal | NQuery of marginal

  module D = L.D

  let gen () =
    let inner = (L.gen_lit (), L.gen_lit (), L.gen_lit ()) in
    PQuery inner

  let neg qry = match qry with
  | PQuery tm -> NQuery tm
  | NQuery tm -> PQuery tm

  let eval_tm row (l1, l2, l3) =
    L.merge_lit (L.eval_lit row l1)
                (L.eval_lit row l2)
                (L.eval_lit row l3)

  let eval_row row qry = match qry with
    | PQuery tm -> tof_att          (eval_tm row tm)
    | NQuery tm -> tof_att (not_att (eval_tm row tm))

end

(* XXX: We could factorize even more *)
module MarLit = struct

  (* 3-way marginal queries over integer attributes *)
  type literal  = LEq of int * int | LNeq of int * int
  module D = IntDb
  let merge_lit = and3_att

  let gen_lit () (* n db_schema *) =
    (* XXX *)
    let att  = 20 in
    let av   = 20 in
    (* let att    = Random.int n                      in *)
    (* let ai     = db_schema.(att)                   in *)
    (* let av     = Random.int (ai.att_max + 1)       in *)
    if Random.bool ()
    then LEq (att, av)
    else LNeq (att, av)

  let eval_lit row lit = match lit with
    | LEq  (n, e) -> row.(n) = e
    | LNeq (n, e) -> row.(n) <> e

end

module MarBLit = struct

  type literal  = PVar of int | NVar of int
  module D = BinDb
  let merge_lit = and3_att

  let gen_lit () =
    (* XXX *)
    let n_att = 20 in
    if Random.bool ()
    then PVar (Random.int n_att)
    else NVar (Random.int n_att)

  let eval_lit row lit = match lit with
  | PVar n ->          row.(n)
  | NVar n -> not_att  row.(n)

end

module ParLit = struct

  (* 3-way parity queries over integer attributes *)
  type literal  = LEq of int * int | LNeq of int * int
  module D = IntDb
  let merge_lit = par3_att

  let gen_lit () (* n db_schema *) =
    (* XXX *)
    let att  = 20 in
    let av   = 20 in
    (* let att    = Random.int n                      in *)
    (* let ai     = db_schema.(att)                   in *)
    (* let av     = Random.int (ai.att_max + 1)       in *)
    if Random.bool ()
    then LEq (att, av)
    else LNeq (att, av)

  let eval_lit row lit = match lit with
    | LEq  (n, e) -> row.(n) = e
    | LNeq (n, e) -> row.(n) <> e

end

module ParBLit = struct

  type literal  = PVar of int | NVar of int
  module D = BinDb
  let merge_lit = par3_att

  let gen_lit () =
    (* XXX *)
    let n_att = 20 in
    if Random.bool ()
    then PVar (Random.int n_att)
    else NVar (Random.int n_att)

  let eval_lit row lit = match lit with
  | PVar n ->          row.(n)
  | NVar n -> not_att  row.(n)

end

module MarQ  = Make(MakeLitOps(MarLit))
module MarBQ = Make(MakeLitOps(MarBLit))
module ParQ  = Make(MakeLitOps(ParLit))
module ParBQ = Make(MakeLitOps(ParBLit))

