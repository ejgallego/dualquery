(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Array
open Attribute

(* A db schema is a list of attributes *)
type db_schema = att_info array

(* A map of regular to binary attributes, (start, size) *)
type att_map   = (int * int)  array

(* Information about the database *)
type db_info = {
  db_name    : string;
  db_att     : int;
  db_bin_att : int;
  db_elem    : int;
}

(* A database is a MxN matrix of attributes for now, an attribute is
   just an integer. How to interpret it depends on the schema.
*)
type db_row = int array
type db     = db_row array

(* Binary database *)
type bin_db_row = bool array
type bin_db     = bin_db_row array

let get_db_att  db = length db.(0)
let get_db_elem db = length db

let get_bin_db_att  db = length db.(0)
let get_bin_db_elem db = length db

(* Build an array of (start, size) for each attribute *)
let build_att_map db_schema =
  let aux = Array.fold_left (fun (n, l) ai ->
                   let sz = ai.att_max + 1 in (n + sz, l @ [(n, sz)]))
    (0, []) db_schema in
  Array.of_list (snd aux)

let mk_db_info name db att_map = {
  db_name     = name;
  db_att      = get_db_att  db;
  db_bin_att  = Array.fold_left (fun v (_, s) -> s + v) 0 att_map;
  db_elem     = get_db_elem db;
}

(* Generate a random database *)
let generate_bool _ = Random.bool ()

let generate_db_elem n_att =
  init n_att generate_bool

let generate_bin_db n_att n_elem =
  init n_elem (fun _ -> generate_db_elem n_att)

let generate_bool_bias bias =
  let r = Random.float 1.0 in
  if r < bias then false else true

let generate_row n_att bias _ =
  init n_att (fun idx -> generate_bool_bias bias.(idx))

let generate_bin_db_bias n_att n_elem =
  let bias = init n_att (fun _ -> Random.float 1.0) in
  (* let bias = init n_att (fun _ -> Random.float 0.2) in *)
  init n_elem (generate_row n_att bias)

(* Better printing will be needed, for now... *)
open Printf

let print_header out db =
  if length db > 0 then
    let atts = length db.(0) in
    fprintf out "    ";
    for i = 0 to atts - 1 do
      fprintf out "%3d" i
    done;
    fprintf out "\n"
  else
    ()

let print_elem out i e =
  fprintf out "%3d:" i;
  iter (fun b -> fprintf out " %s" (if b then " 1" else " 0")) e;
  fprintf out "\n"

let print_db out db =
  (* We don't want the header I guess *)
  (* print_header out db; *)
  iteri (print_elem out) db

let test_db () =
  print_db stdout (generate_bin_db 20 10)
