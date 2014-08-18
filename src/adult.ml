(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Attribute

open List
open Str

let parsing_schema =
  (*            name           min max    def step *)
  [| mk_int_att "age"            0 100     10 20;
     mk_int_att "workclass"      0 7        0  8;
     mk_int_att "fnlwgt"         0 1600000  0 20;
     mk_int_att "education"      0 15       0 16;
     mk_int_att "ed_num"         0 100      0 10;

     mk_int_att "marital_status" 0 6        0  7;
     mk_int_att "occupation"     0 13       0 14;
     mk_int_att "relationship"   0 5        0 60;
     mk_int_att "race"           0 4        0  5;
     mk_int_att "sex"            0 1        0  2;
     mk_int_att "capital_gain"   0 100000   0 10;
     mk_int_att "capital_loss"   0 5000     0 10;
     mk_int_att "hours_per_week" 0 100      0 10;
     mk_int_att "native_country" 0 40       0 41;
     mk_int_att "income"         0 1        0  2;
  |]

let parsing_schema_reduced =
  (*            name           min max    def step *)
  [| mk_int_att "age"            0 100      0 10;
     mk_int_att "workclass"      0 7        0  3;
     mk_int_att "fnlwgt"         0 1600000  0  3;
  |]
(*
     mk_int_att "education"      0 15       0 16;
     mk_int_att "ed_num"         0 100      0 10;

     mk_int_att "marital_status" 0 6        0  7;
     mk_int_att "occupation"     0 13       0 14;
     mk_int_att "relationship"   0 5        0 60;
     mk_int_att "race"           0 4        0  5;
     mk_int_att "sex"            0 1        0  2;
     mk_int_att "capital_gain"   0 100000   0 10;
     mk_int_att "capital_loss"   0 5000     0 10;
     mk_int_att "hours_per_week" 0 100      0 10;
     mk_int_att "native_country" 0 40       0 41;
     mk_int_att "income"         0 1        0  2;
  |]
*)

let schema         = Array.map gen_att_info parsing_schema
let schema_reduced = Array.map gen_att_info parsing_schema_reduced

let parse_entry sch n line =
  let s_line = Str.split (regexp_string ",") line                          in
  let entry  = Array.init n
    (fun i ->
      (* Printf.printf "%s%!\n" (nth s_line i); *)
      parse_attribute sch.(i) (nth s_line i)) in
  entry

let parse_res : ((int array) list) ref = ref []
let parse_iter                         = ref 0

let rec parse_loop sch ic =
  let n = Array.length sch in
  try
    while true do
      parse_iter := (!parse_iter) + 1;

      let line  = input_line ic            in
      let pline = parse_entry sch n line   in
      parse_res := pline :: !parse_res
    done
  with End_of_file ->
    close_in ic

let parse_file sch file =
  let ic = open_in file in
  parse_loop sch ic;
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

let read_db sch file () =
  parse_res  := [];
  parse_iter := 0 ;
  let l_db  = parse_file sch file                  in
  let a_db  = Array.of_list l_db                   in
  (* ann_db schema a_db; *)
  a_db
