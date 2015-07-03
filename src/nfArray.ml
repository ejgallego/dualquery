(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Binarray
module Ni = Nativeint

open Str
open List

(* Array of user movies *)

let nf_entries = 480189
(* let nf_entries = 50000 *)
let nf_atts    = 17770

let rec put_array db idx list = match list with
    []        -> ()
  | (x :: xs) -> att_set db idx x;
                 put_array db idx xs

(* We get the variable number of movies and ouput a set *)
let parse_entry db i line =
  let s_line = Str.split (regexp_string ",") line     in
  let n_line = map int_of_string s_line               in
  put_array db i n_line

let rec parse_loop db ic =

  let parse_iter = ref 0 in
  (
    try
      while true do
	if !parse_iter mod 1000 = 0 then
	  Printf.printf "User: %d \n%!" !parse_iter;

	let line   = input_line ic            in
	parse_entry db !parse_iter line;
	parse_iter := (!parse_iter) + 1
      done
    with End_of_file ->
      close_in ic
  );
  !parse_iter

let read_db file =
  let db        = make nf_entries nf_atts Ni.zero in
  let ic        = open_in file                    in
  let n_entries = parse_loop db ic                in
  Bigarray.Array2.sub_left db 0 n_entries

open Db
open Dq
open Query
open Printf

open Bineval

let mk_db_info elem = {
  db_name    = "netflix";
  db_att     = nf_atts;
  db_bin_att = nf_atts;
  db_elem    = elem;
}

let mk_netflix_exp_data file nqry () =
  let db       = read_db file                        in
  let elem     = dim1 db                             in
  let b_qry    = generate_bqueries nf_atts nqry      in
  let qcache   = ba_eval_bqueries_norm true db b_qry in
  let c_bqry   = complement_bqueries b_qry           in
  let c_qcache = complement_qcache  qcache           in
  {
    sd_info    = mk_db_info elem;
    sd_queries = c_bqry;
    sd_qcache  = c_qcache;
  }

