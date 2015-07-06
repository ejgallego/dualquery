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

  (* Print to cplex, needs global query number for cplex var *)
  val pp_cplex : Format.formatter -> int -> query -> unit
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

  let pp_cplex = O.pp_cplex
end

(* 3-way Marginals and Parities *)

(* Binary Literal Ops *)

(*

things in common:

    val eval_lit  : Diff: B/I Eq:   P/M
    val merge_lit : Eq: B/I   Diff: P/M

    val pp_cplex: Diff P/M at the level of Q

    rest: Eq

    Thus makeLitOps has to distinguish

 Solution, have a base type with query?

 Eval is the same for par

*)

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

(* Common Helpers *)
let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c
let par3_att a b c =  (a && b && not c) ||     (c && a && not b)
                   || (b && c && not a) || not (a || b ||     c)

let string_of_sgn   b = if b then "+" else "-"
let not_int_of_bool b = if b then 0 else 1
let var_sgn b1 b2     = b1 = b2

module MakeLitOps (L : BinLitOps) : Ops = struct

  type marginal = L.marginal
  type query    = L.query

  module D = BinDb

  let gen () =
    let inner = (L.gen_lit (), L.gen_lit (), L.gen_lit ()) in
    L.PQuery inner

  let neg qry = match qry with
  | L.PQuery tm -> L.NQuery tm
  | L.NQuery tm -> L.PQuery tm

  let eval_tm row (l1, l2, l3) =
    L.merge_lit (L.eval_lit row l1)
                (L.eval_lit row l2)
                (L.eval_lit row l3)

  let eval_row row qry = match qry with
    | L.PQuery tm -> tof_att          (eval_tm row tm)
    | L.NQuery tm -> tof_att (not_att (eval_tm row tm))


  (* XXX: Should we use include? *)
  let pp_cplex = L.pp_cplex
end

module MarBLit = struct

  type literal  = PVar of int | NVar of int
  type marginal = literal * literal * literal
  type query    = PQuery of marginal | NQuery of marginal

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

  (* CPlex is quite picky about the input format, so we must
   * "normalize" the queries prior to writing them, such that no
   * constant in on the left side. Recall that constants on the left
   * side are introduced by the negated literals
   *)

  let decomp_literal l = match l with
    | PVar i -> (true,  i)
    | NVar i -> (false, i)

  (* Return the sign and number of variables, the query factor and the corrected right side *)
  let norm_query q = match q with
    | PQuery (l1, l2, l3) ->
       let (l1s, l1i) = decomp_literal l1 in
       let (l2s, l2i) = decomp_literal l2 in
       let (l3s, l3i) = decomp_literal l3 in
       let ls = var_sgn true              in
       let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
       (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, 3, -correction_factor)

    | NQuery (l1, l2, l3) ->
       let (l1s, l1i) = decomp_literal l1 in
       let (l2s, l2i) = decomp_literal l2 in
       let (l3s, l3i) = decomp_literal l3 in
       let ls = var_sgn false             in
       let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
       (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, 1, -3+correction_factor)

  let pp_cplex fmt qnum qry =
    let (l1s, l1i, l2s, l2i, l3s, l3i, qf, rs) = norm_query qry in
    Format.fprintf fmt "%s x%d %s x%d %s x%d - %dq%d >= %d\n"
            (string_of_sgn l1s) l1i
            (string_of_sgn l2s) l2i
            (string_of_sgn l3s) l3i
            qf qnum rs

end

module ParBLit = struct

  type literal  = PVar of int | NVar of int
  type marginal = literal * literal * literal
  type query    = PQuery of marginal | NQuery of marginal

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

  let decomp_literal l = match l with
    | PVar i -> (true,  i)
    | NVar i -> (false, i)

  (* Return the sign and number of variables, the query factor and the
     corrected right side *)

  let norm_query_p q = match q with
  | PQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn true              in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, -correction_factor - 1)

  | NQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn false             in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, correction_factor)

  let pp_cplex fmt qnum qry =
    let (l1s, l1i, l2s, l2i, l3s, l3i, rs) = norm_query_p qry in
    Format.fprintf fmt "%s x%d %s x%d %s x%d - 2p%d - 2r%d - q%d = %d\n"
    (string_of_sgn l1s) l1i
    (string_of_sgn l2s) l2i
    (string_of_sgn l3s) l3i
    qnum qnum qnum rs

end

module MarBQ = Make(MakeLitOps(MarBLit))
module ParBQ = Make(MakeLitOps(ParBLit))

(* Non-binary cases disabled for now *)

(* module MarQ  = Make(MakeLitOps(MarLit)) *)
(* module ParQ  = Make(MakeLitOps(ParLit)) *)

(*
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
*)
