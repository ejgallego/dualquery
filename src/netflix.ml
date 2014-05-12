(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

module A = Array
open List
open Str

module UserMovies = Set.Make(
  struct
    type t = int
    let compare = Pervasives.compare
  end )

(* Array of user movies *)
type nf_db = UserMovies.t array

(* let nf_entries = 480189 *)
(* let nf_db      = A.make nf_entries UserMovies.empty *)

let list_to_set li =
  List.fold_left (fun set elem -> UserMovies.add elem set) UserMovies.empty li

(* We get the variable number of movies and ouput a set *)
let parse_entry line =
  let s_line = Str.split (regexp_string ",") line     in
  let n_line = map int_of_string s_line               in
  list_to_set n_line

let parse_res : (UserMovies.t list) ref = ref []

let rec parse_loop ic =

  let parse_iter = ref 0 in
  try
    while true do
      if !parse_iter mod 1000 = 0 then
	Printf.printf "User: %d \n%!" !parse_iter;

      let line   = input_line ic            in
      let user_m = parse_entry line         in
      parse_res := user_m :: !parse_res;
      (* A.set nf_db !parse_iter user_m; *)

      parse_iter := (!parse_iter) + 1;
    done
  with End_of_file ->
    close_in ic

let parse_file file =
  let ic = open_in file                     in
  parse_loop ic;
  (* nf_db *)
  Array.of_list !parse_res

let read_db file =
  let a_db  = parse_file file               in
  a_db

open Db
open Dq
open Query
open Printf

(* MovieIDs range from 1 to 17770 sequentially. *)

let nf_atts = 17770

let mk_db_info elem = {
  db_att     = nf_atts;
  db_bin_att = nf_atts;
  db_elem    = elem;
}

(* FIXME: We should define a module, etc... and integrate this in query.ml *)
let tof_att  b     = if b then 1.0 else 0.0
let not_att  a     = not a
let and3_att a b c = a && b && c

let eval_nf_lit (elem : UserMovies.t) lit = match lit with
  | PVar n ->           UserMovies.mem n elem
  | NVar n -> not_att  (UserMovies.mem n elem)

let eval_nf_tm (elem : UserMovies.t) (l1, l2, l3) =
  and3_att (eval_nf_lit elem l1)
           (eval_nf_lit elem l2)
           (eval_nf_lit elem l3)

(* We can improve performnce on this *)
let eval_bin_elem query (elem : UserMovies.t) = match query with
  | BPQuery tm -> tof_att          (eval_nf_tm elem tm)
  | BNQuery tm -> tof_att (not_att (eval_nf_tm elem tm))

let eval_nf_query (db : nf_db) query =
  Array.fold_left (fun qry_val -> fun db_e -> qry_val +. (eval_bin_elem query db_e)) 0.0 db

(* Perform the normalization in place *)
let ev_norm n res =
  let n = float_of_int n in
  Util.mapi_in_place (fun idx -> fun r -> r /. n) res

let eval_nf_queries_norm verbose db queries =
  let db_length = Array.length db         in
  let n_queries = Array.length queries    in
  let p_time = ref (Unix.gettimeofday ()) in

  ev_norm db_length
  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (* (Parmap.array_float_parmapi ~ncores:n_cores (fun idx q -> *)
    (Parmap.array_parmapi ~ncores:Params.n_cores (fun idx q ->
    (* (Array.mapi (fun idx -> fun q -> *)
    let report = 10 in
    if verbose && (idx mod report) = 0 then
      let a_time    = Unix.gettimeofday ()                        in
      let secs      = (a_time -. !p_time)                         in
      let q_by_s    = (float_of_int report) /. secs               in
      let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)    in
      p_time       := a_time;
      Printf.printf "*** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
    else
      ();
    eval_nf_query db q) queries)

let mk_netflix_exp_data file nqry =
  let db       = read_db file                       in
  let elem     = A.length db                        in
  let b_qry    = generate_bqueries nf_atts nqry     in
  let qcache   = eval_nf_queries_norm true db b_qry in
  let c_bqry   = complement_queries b_qry           in
  let c_qcache = complement_qcache  qcache          in
  {
    sd_name    = sprintf "netflix[%d]-q%d" elem nqry ;
    sd_info    = mk_db_info elem;
    sd_queries = c_bqry;
    sd_qcache  = c_qcache;
  }
