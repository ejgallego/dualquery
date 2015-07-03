(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open List
open Str

open Attribute

let parsing_schema = [|
  mk_int_att "A" 0 1 0 2;
  mk_int_att "B" 0 1 0 2;
  mk_int_att "C" 0 1 0 2;
  mk_int_att "D" 0 1 0 2;
  mk_int_att "E" 0 1 0 2;

  mk_int_att "F" 0 1 0 2;
  mk_int_att "G" 0 1 0 2;
  mk_int_att "H" 0 1 0 2;
  mk_int_att "I" 0 1 0 2;
  mk_int_att "J" 0 1 0 2;

  mk_int_att "K" 0 1 0 2;
  mk_int_att "L" 0 1 0 2;
  mk_int_att "M" 0 1 0 2;
  mk_int_att "N" 0 1 0 2;
  mk_int_att "O" 0 1 0 2;

  mk_int_att "P" 0 1 0 2;
  mk_int_att "Q" 0 1 0 2;
  mk_int_att "R" 0 1 0 2;
  mk_int_att "S" 0 1 0 2;
  mk_int_att "T" 0 1 0 2;

  mk_int_att "U" 0 1 0 2;
  mk_int_att "V" 0 1 0 2;
  mk_int_att "W" 0 1 0 2;
  mk_int_att "X" 0 1 0 2;
  mk_int_att "Y" 0 1 0 2;

  mk_int_att "Z" 0 1 0 2;
  mk_int_att "AA" 0 1 0 2;
  |]

let schema = Array.map gen_att_info parsing_schema

let parse_entry n line =
  let s_line = Str.split (regexp_string " ") line                          in
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

      let line  = input_line ic        in
      let pline = parse_entry n line   in
      parse_res := pline :: !parse_res
    done
  with End_of_file ->
    close_in ic

let parse_file file =
  let ic = open_in file in
  parse_loop ic;
  !parse_res

let read_db file () =
  let l_db  = parse_file    file                   in
  let a_db  = Array.of_list l_db                   in
  (* ann_db schema a_db; *)
  a_db

(**********************************************************************)
(* Old code *)

(* FIXME: We are ignoring the last column which is -1 or 1 *)
let ttt_exp = regexp
  "\\([01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01] [01]\\)"

(* Old code *)

let ttt_att  = 27
let ttt_elem = 958

let ttt_bool str i =
  if (String.get str i = '1') then true else false

let rec ttt_set db idx atts =
  for i = 0 to (ttt_att - 1) do
    Array.set (Array.get db idx) i (ttt_bool atts (i * 2))
  done

let ttt_split_and_set db s_line idx =
  if Str.string_match ttt_exp s_line 0 then
    let atts = Str.matched_group 1 s_line in
    (* Printf.printf "%03d: %s \n%!" idx atts; *)
    ttt_set db idx atts
  else
    raise Not_found

let rec read_loop ic db idx =
  try
    let line = input_line ic in
    (* Printf.printf "%s %!" line; *)
    ttt_split_and_set db line idx;
    read_loop ic db (idx + 1)
  with End_of_file -> close_in ic

