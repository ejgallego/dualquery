(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Attribute

open List
open Str

let parsing_schema = [|
  mk_int_att   "duration"     0     60000  0 20;
  mk_int_att   "protocol"     0         2  1  3;
  mk_int_att   "service"      0        65 22 65;
  mk_int_att   "flag"         0        10  9 11;
  mk_int_att   "src_bytes"    0 700000000  0 20;
  mk_int_att   "dest_bytes"   0   6000000  0 20;
  mk_int_att   "land"         0         1  0  2;
  mk_int_att   "wrong"        0         3  0  4;
  mk_int_att   "urgent"       0         3  0  4;
  mk_int_att   "hot"          0        30  0 10;
  mk_int_att   "failed"       0         5  0  6;
  mk_int_att   "logged"       0         1  0  2;
  mk_int_att   "compro"       0       900  0  10;
  mk_int_att   "root"         0         1    0  2;
  mk_int_att   "su"           0         2    0  3;
  mk_int_att   "num_root"     0      1000 0  5;
  mk_int_att   "create"       0        30   0  5;
  mk_int_att   "num_shell"    0         2    0  3;
  mk_int_att   "access"       0        10   0  5;
  mk_int_att   "outbound"     0       1    0  2;
  mk_int_att   "is_host"      0       1    0  2;
  mk_int_att   "is_guest"     0       1    0  2;
  mk_int_att   "count"        0       600  0 10;
  mk_int_att   "srv_count"    0       600  0 10;
  mk_float_att "serror"       0.0     1.0 0.0 10;
  mk_float_att "srv_serror"   0.0     1.0 0.0 10;
  mk_float_att "rerror"       0.0     1.0 0.0 10;
  mk_float_att "srv_rerror"   0.0     1.0 0.0 10;
  mk_float_att "same"         0.0     1.0 0.0 10;
  mk_float_att "diff"         0.0     1.0 0.0 10;
  mk_float_att "srv_diff"     0.0     1.0 0.0 10;
  mk_int_att   "host_count"   0   255   0 10;
  mk_int_att   "d_same"       0   255   0 10;
  mk_float_att "d_diff"       0.0 1.0 0.0 10;
  mk_float_att "d_same_port"  0.0 1.0 0.0 10;
  mk_float_att "d_diff_host"  0.0 1.0 0.0 10;
  mk_float_att "d_serror"     0.0 1.0 0.0 10;
  mk_float_att "d_srv_serror" 0.0 1.0 0.0 10;
  mk_float_att "d_rerror"     0.0 1.0 0.0 10;
  mk_float_att "d_srv_rerror" 0.0 1.0 0.0 10;
  mk_float_att "attack"       0.0 1.0 0.0 10;
  |]

let schema = Array.map gen_att_info parsing_schema

let parse_entry n line =
  let s_line = Str.split (regexp_string ", ") line                         in
  let entry  = Array.init n
    (fun i -> parse_attribute (Array.get parsing_schema i) (nth s_line i)) in
  entry

let parse_res : ((int array) list) ref = ref []
let parse_iter                         = ref 0

let rec parse_loop ic =
  let n = Array.length parsing_schema in
  try
    while true do
      parse_iter := (!parse_iter) + 1;
      (* Printf.printf "%d %!" !parse_iter; *)

      let line  = input_line ic              in
      let pline = parse_entry n line in
      parse_res := pline :: !parse_res
    done
  with End_of_file ->
    close_in ic

let parse_file file =
  parse_res  := [];
  parse_iter :=  0;
  let ic = open_in file in
  parse_loop ic;
  !parse_res

let compare_elem  i (min, max, n, sum) =
    Printf.printf "Element %s: real max %d vs spec %d, real avg %d vs spec %d\n"
      i.att_name max i.att_max (sum / n) i.att_def

let compare_stats s stats =
  Array.iteri (fun idx e -> compare_elem s.(idx) e) stats

let db_stats s_arr elem =
  Array.mapi (fun idx (min_s,max_s,n,a) ->
    let v = elem.(idx) in
    (min min_s v, max max_s v, n+1, a + v)) s_arr

let ann_db s db =
  (* Generate the stats *)
  let init_s = Array.make (Array.length s) (max_int, min_int, 0, 0) in
  let stats  = Array.fold_left db_stats init_s db                   in
  compare_stats s stats

let read_db file () =
  let l_db  = parse_file file                   in
  let a_db  = Array.of_list l_db                in
  ann_db schema a_db;
  a_db
