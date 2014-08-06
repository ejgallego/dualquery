(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Query evaluation for binarrays *)

open Binarray
open Query

module Ni = Nativeint

(* FIXME: We should define a module, etc... and integrate this in query.ml *)
let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c

let ba_eval_lit db row lit = match lit with
  | PVar n ->           att_get db row n
  | NVar n -> not_att  (att_get db row n)

let ba_eval_tm db row (l1, l2, l3) =
  and3_att (ba_eval_lit db row l1)
           (ba_eval_lit db row l2)
           (ba_eval_lit db row l3)

(* We can improve performance on this *)
let ba_eval_elem query db row = match query with
  | BPQuery tm -> tof_att          (ba_eval_tm db row tm)
  | BNQuery tm -> tof_att (not_att (ba_eval_tm db row tm))

let ba_eval_query (db : ba) query =
  let fold f x a =
    let r = ref x in
    for i = 0 to dim1 a - 1 do
      r := f !r i
    done;
  !r in
  fold (fun qry_val -> fun i -> qry_val +. (ba_eval_elem query db i)) 0.0 db

(* Perform the normalization in place *)
let ev_norm_in_place n res =
  let n = float_of_int n in
  Util.map_in_place (fun r -> r /. n) res

let ba_eval_bqueries_norm verbose db queries =
  let db_length = dim1 db                    in
  let n_queries = Array.length queries       in
  let p_time    = ref (Unix.gettimeofday ()) in


  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (* (Parmap.array_float_parmapi ~ncores:n_cores (fun idx q -> *)
  let res =
    (Parmap.array_parmapi ~ncores:Params.n_cores (fun idx q ->
    (* (Array.mapi (fun idx -> fun q -> *)
    let report = 100 in
    if verbose && (idx mod report) = 0 then
      let a_time    = Unix.gettimeofday ()                                    in
      let secs      = (a_time -. !p_time)                                     in
      let q_by_s    = (float_of_int report) /. secs                           in
      let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)  in
      p_time       := a_time;
      Printf.printf "*** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
    else
      ();
    ba_eval_query db q) queries)
  in
  ev_norm_in_place db_length res;
  res

let ba_eval_bqueries_norm_slow verbose db queries =
  let db_length = dim1 db                    in
  let n_queries = Array.length queries       in
  let p_time    = ref (Unix.gettimeofday ()) in

  let res =
  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (Array.mapi (fun idx -> fun q ->
      let report = 100 in
      if verbose && (idx mod report) = 0 then
	let a_time    = Unix.gettimeofday ()                                    in
	let secs      = (a_time -. !p_time)                                     in
	let q_by_s    = (float_of_int report) /. secs                           in
	let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)  in
	p_time       := a_time;
	Printf.printf "*** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
      else
	();
      ba_eval_query db q) queries)
  in
  ev_norm_in_place db_length res;
  res


let ba_generate_bool_bias db row att bias =
  let r = Random.float 1.0               in
  if r < bias then
    ()
  else
    att_set db row att

let ba_generate_row db row n_att bias =
  for i = 0 to n_att - 1 do
    ba_generate_bool_bias db row i (Array.get bias i)
  done

let ba_generate_bin_db_bias n_att n_elem =
  let db   = make n_elem n_att Ni.zero in
  let bias = Array.init n_att (fun _ -> Random.float 1.0) in
  for i = 0 to n_elem - 1 do
    ba_generate_row db i n_att bias
  done;
  db

module Q = Query
open Db
open Dq

let mk_ba_rbias_data elem atts nqry =
    let db       = ba_generate_bin_db_bias atts elem    in
    let qry      = Q.generate_bqueries     atts nqry    in
    let qcache   = ba_eval_bqueries_norm true db qry    in
    let c_qry    = Q.complement_bqueries qry            in
    let c_qcache = Q.complement_qcache   qcache         in
    {
      sd_info    = { db_name = "ba_ran_bias"; db_att = atts; db_bin_att = atts; db_elem = elem; };
      sd_queries = c_qry;
      sd_qcache  = c_qcache;
    }

let mk_ba_rbias_data_slow elem atts nqry =
    let db       = ba_generate_bin_db_bias atts elem       in
    let qry      = Q.generate_bqueries     atts nqry       in
    let qcache   = ba_eval_bqueries_norm_slow true db qry  in
    let c_qry    = Q.complement_bqueries qry               in
    let c_qcache = Q.complement_qcache   qcache            in
    {
      sd_info    = { db_name = "ba_ran_bias"; db_att = atts; db_bin_att = atts; db_elem = elem; };
      sd_queries = c_qry;
      sd_qcache  = c_qcache;
    }
