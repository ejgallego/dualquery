(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Attribute
open Db
open Printf

(* Generic query *)
type 'a gen_query = {
  q_     : 'a array;
  q_gen  : db_schema -> 'a;
  q_eval : 'a -> bin_db_row -> float;
}

(* Queries are 3-way marginals, negated or not *)

(* Integer queries *)
type int_literal    = LEq of int * int | LNeq of int * int
type int_3_marginal = int_literal * int_literal * int_literal
type query          = PQuery of int_3_marginal | NQuery of int_3_marginal

(* Note on query generation: we may generate the same query twice, but
   it is not very concerning *)
let generate_iliteral n db_schema =
  let att    = Random.int n                      in
  let ai     = db_schema.(att)                   in
  let av     = Random.int (ai.att_max + 1)       in
  if Random.bool () then
    LEq (att, av)
  else
    LNeq (att, av)

let generate_iquery n db_schema =
  let inner = (generate_iliteral n db_schema,
               generate_iliteral n db_schema,
               generate_iliteral n db_schema) in
  PQuery inner

let generate_iqueries db_schema n_q =
  let n = Array.length db_schema in
  Array.init n_q (fun _ -> generate_query n db_schema)

let eval_bin_query (db : bin_db) query =
  (* let e_arry = Array.map (eval_elem query) db in *)
  Array.fold_left (fun qry_val -> fun db_e -> qry_val +. (eval_bin_elem query db_e)) 0.0 db

let eval_bin_queries db queries =
  Array.map (eval_bin_query db) queries

let eval_lit (elem : db_row) lit = match lit with
  | LEq  (n, e) -> elem.(n) = e
  | LNeq (n, e) -> elem.(n) <> e

let eval_tm (elem : db_row) (l1, l2, l3) =
  and3_att (eval_lit elem l1)
           (eval_lit elem l2)
           (eval_lit elem l3)

let eval_elem query (elem : db_row) = match query with
  | PQuery tm -> tof_att          (eval_tm elem tm)
  | NQuery tm -> tof_att (not_att (eval_tm elem tm))

let eval_query (db : db) query =
  (* let e_arry = Array.map (eval_elem query) db in *)
  Array.fold_left (fun qry_val -> fun db_e -> qry_val +. (eval_elem query db_e)) 0.0 db

(* let ev_norm n res = *)
(*   Array.map (fun r -> (float_of_int r) /. (float_of_int n)) res *)

(* Perform the normalization in place *)
let ev_norm_in_place n (dist : float array) : unit =
  let n = float_of_int n in
  Util.map_in_place (fun r -> r /. n) dist

let eval_queries_norm verbose db queries =
  let db_length = Array.length db         in
  let n_queries = Array.length queries    in
  let p_time = ref (Unix.gettimeofday ()) in

  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (* (Parmap.array_float_parmapi ~ncores:n_cores (fun idx q -> *)
  let res =
    (Parmap.array_parmapi ~ncores:Params.n_cores (fun idx q ->
    (* (Array.mapi (fun idx -> fun q -> *)
    let report = 1000 in
    if verbose && (idx mod report) = 0 then
      let a_time    = Unix.gettimeofday ()                        in
      let secs      = (a_time -. !p_time)                         in
      let q_by_s    = (float_of_int report) /. secs               in
      let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)    in
      p_time       := a_time;
      Printf.printf "**** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
    else
      ();
    eval_query db q) queries)
  in
  ev_norm_in_place db_length res;
  res


(* Binary special case *)
type bin_literal        = PVar of int | NVar of int
type bin_three_marginal = bin_literal * bin_literal * bin_literal
type bin_query          = BPQuery of bin_three_marginal | BNQuery of bin_three_marginal


let generate_bliteral n_att =
  if Random.bool () then
    PVar (Random.int n_att)
  else
    NVar (Random.int n_att)

let generate_bquery n_att =
  let inner = (generate_bliteral n_att, generate_bliteral n_att, generate_bliteral n_att) in
  BPQuery inner

let generate_bqueries n_atts n_q = Array.init n_q (fun _ -> generate_bquery n_atts);

let generate_bqueries n_atts n_q =
  { q_              = generate_bqueries n_atts n_q;
    q_gen db_schema = generate_bquery 2;
    q_eval : 'a -> bin_db_row -> float;


(* Naive *)
(* let generate_lin_att s a = *)
(*   if s = 1 then *)
(*     PVar a *)
(*   else *)
(*     NVar a *)

(* let generate_lin_query n_att n = *)
(*   let q1 = n mod (n_att * 2)                         in *)
(*   let p1 = q1 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let q2 = n mod (n_att * 2)                         in *)
(*   let p2 = q2 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let q3 = n mod (n_att * 2)                         in *)
(*   let p3 = q3 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let qs = n mod 2                                   in *)
(*   let inner = (generate_lin_att p1 (q1 / 2), generate_lin_att p2 (q2 / 2), generate_lin_att p3 (q3 / 2)) in *)
(*   if qs = 1 then *)
(*     NQuery inner *)
(*   else *)
(*     PQuery inner *)

(* let generate_lin_queries n_atts n_q = *)
(*   Array.init n_q (generate_lin_query n_atts) *)

(* (\* Note that this only generates the positive queries *\) *)
(* let generate_all_queries n_atts = *)
(*   generate_lin_queries n_atts (n_atts * n_atts * n_atts * 2 * 2 * 2) *)

let to_bin_literal att_map lit = match lit with
  | LEq  (a, v) ->
    let (a_pos, ne) = att_map.(a) in
    (* Little hack to support binary attributes *)
    if ne = 0 then
      (if v = 1 then PVar a_pos else NVar a_pos)
    else
      PVar (a_pos + v)

  | LNeq (a, v) ->
    let (a_pos, ne) = att_map.(a) in
    (* Little hack to support binary attributes *)
    if ne = 0 then
      (if v = 1 then NVar a_pos else PVar a_pos)
    else
      NVar (a_pos + v)

let to_bin_query att_map query = match query with
  | PQuery (l1, l2, l3) ->
    let bl1 = to_bin_literal att_map l1 in
    let bl2 = to_bin_literal att_map l2 in
    let bl3 = to_bin_literal att_map l3 in
    BPQuery (bl1, bl2, bl3)

  | NQuery (l1, l2, l3) ->
    let bl1 = to_bin_literal att_map l1 in
    let bl2 = to_bin_literal att_map l2 in
    let bl3 = to_bin_literal att_map l3 in
    BNQuery (bl1, bl2, bl3)

let to_bin_queries att_map qry =
  Array.map (to_bin_query att_map) qry

let complement_query q = match q with
  | PQuery tm -> NQuery tm
  | NQuery tm -> PQuery tm

let complement_bquery q = match q with
  | BPQuery tm -> BNQuery tm
  | BNQuery tm -> BPQuery tm

let complement_queries queries =
  let c_queries = Array.map complement_query queries in
  Array.append queries c_queries

let complement_bqueries queries =
  let c_queries = Array.map complement_bquery queries in
  Array.append queries c_queries

let complement_qcache  qcache =
  let c_qcache  = Array.map (fun v -> 1.0 -. v) qcache in
  Array.append qcache c_qcache

(* Get the first half of an array, that is, without the complements *)
let remove_complements q =
  let l = Array.length q / 2 in
  Array.sub q 0 l

let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c

let eval_bin_lit (elem : bin_db_row) lit = match lit with
  (* We must use the .() notation for -unsafe to work *)
  | PVar n ->          elem.(n)
  | NVar n -> not_att  elem.(n)

let eval_btm (elem : bin_db_row) (l1, l2, l3) =
  and3_att (eval_bin_lit elem l1)
           (eval_bin_lit elem l2)
           (eval_bin_lit elem l3)

(* let bool_to_float b = if b then 1.0 else 0.0 *)

(* We can improve performance on this *)
let eval_bin_elem query (elem : bin_db_row) = match query with
  | BPQuery tm -> tof_att          (eval_btm elem tm)
  | BNQuery tm -> tof_att (not_att (eval_btm elem tm))


let eval_bqueries_norm verbose db queries =
  let db_length = Array.length db         in
  let n_queries = Array.length queries    in
  let p_time = ref (Unix.gettimeofday ()) in

  let res =
  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (* (Parmap.array_float_parmapi ~ncores:n_cores (fun idx q -> *)
    (Parmap.array_parmapi ~ncores:Params.n_cores (fun idx q ->
    (* (Array.mapi (fun idx -> fun q -> *)
    let report = 1000 in
    if verbose && (idx mod report) = 0 then
      let a_time    = Unix.gettimeofday ()                        in
      let secs      = (a_time -. !p_time)                         in
      let q_by_s    = (float_of_int report) /. secs               in
      let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)    in
      p_time       := a_time;
      Printf.printf "**** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
    else
      ();
    eval_bin_query db q) queries)
  in
  ev_norm_in_place db_length res;
  res


let string_of_lt l = match l with
  | PVar n -> sprintf " %3d" n
  | NVar n -> sprintf "!%3d" n

let string_of_tm (l1, l2, l3) =
  sprintf "%s /\\ %s /\\ %s"
    (string_of_lt l1)
    (string_of_lt l2)
    (string_of_lt l3)

let print_query i q = match q with
  | BPQuery tm -> printf "%3d: + %s\n" i (string_of_tm tm)
  | BNQuery tm -> printf "%3d: - %s\n" i (string_of_tm tm)

let print_bqueries q =
  Array.iteri print_query q

(*  *)
(* let test_eval () = *)
(*   let n_att   = 30                        in *)
(*   let db      = generate_db n_att 20      in *)
(*   let queries = generate_queries n_att 10 in *)
(*   print_db db; *)
(*   print_queries queries; *)
(*   eval_queries db queries *)

