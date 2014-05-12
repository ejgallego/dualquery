(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Dq
open Query

open Printf
open Scanf

let version = 2
exception QCacheError

let print_var oc v =
  let (s, a) = (match v with
    | PVar v -> ('+', v)
    | NVar v -> ('-', v)) in
  fprintf oc "%c %d " s a

let print_query oc q =
  let (s, (m1, m2, m3)) = (match q with
    | BPQuery m -> ('P', m)
    | BNQuery m -> ('N', m)) in
  fprintf oc "%c " s;
  print_var oc m1;
  print_var oc m2;
  print_var oc m3

let save_queries oc q qc =
  Array.iteri (fun idx q ->
    print_query oc q;
    fprintf oc "%.20f\n" qc.(idx)
  ) q

let save_exp_data d file =
  let oc = open_out file in
  let b  = d.sd_info     in

  fprintf oc "%d\n"       version;
  fprintf oc "%s\n"       b.db_name;
  fprintf oc "%d %d %d\n" b.db_att b.db_bin_att b.db_elem;

  let queries = remove_complements d.sd_queries in
  let qcache  = remove_complements d.sd_qcache  in

  save_queries oc queries qcache;

  close_out oc

let save_only_queries oc q =
  Array.iteri (fun idx q ->
    print_query oc q;
    fprintf oc "\n"
  ) q

let save_exp_queries d file =
  let oc      = open_out file                   in
  let queries = remove_complements d.sd_queries in

  save_only_queries oc queries;

  close_out oc

let build_var s v = match s with
  | '+' -> PVar v
  | '-' -> NVar v
  | _   -> raise QCacheError

let read_query ic =
  fscanf ic "%c %c %d %c %d %c %d %f\n" (fun s s1 v1 s2 v2 s3 v3 qc ->
    let (m1, m2, m3) = (build_var s1 v1, build_var s2 v2, build_var s3 v3) in
    match s with
    | 'P' -> (BPQuery (m1, m2, m3), qc)
    | 'N' -> (BNQuery (m1, m2, m3), qc)
    | _   -> raise QCacheError
  )

let load_exp_data ?cutoff file =
  let ic = open_in file in

  fscanf ic "%d\n" (fun i -> if i <> version then raise QCacheError else ());
  let (name, att, bin_att, elem) =
    fscanf ic "%s\n%d %d %d\n" (fun n a b e -> (n, a, b, e))   in

  let parse_iter = ref 0  in
  let q_list     = ref [] in
  let qc_list    = ref [] in

  let cond i = match cutoff with
    | None   -> true
    | Some n -> i < n
  in

  begin
    try
      while cond !parse_iter do
	parse_iter := (!parse_iter) + 1;

	(* let line    = input_line ic              in *)
	(* printf "%s\n%!" line; *)

	let (q, qc) = read_query ic            in
	q_list     := q  :: !q_list;
	qc_list    := qc :: !qc_list
      done
    with
    | End_of_file -> close_in ic
  end;

  let queries = complement_bqueries (Array.of_list (List.rev !q_list))  in
  let qcache  = complement_qcache   (Array.of_list (List.rev !qc_list)) in
  {
    sd_info    = { db_name = name; db_att = att; db_bin_att = bin_att; db_elem = elem; };
    sd_queries = queries;
    sd_qcache  = qcache;
  }

