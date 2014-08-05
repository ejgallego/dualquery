(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Query

(* A database represented as a histogram  *)
type db_dist = float array

let int_of_bool b = if b then 1 else 0

let elem_to_index e =
  Array.fold_left (fun n b ->
    (n lsl 1) lor (int_of_bool b)) 0 e

(* Convert a database to its histogram *)
let to_dist dbi db =
  let size  = Util.pow 2 dbi.db_bin_att  in

  (* Careful with exponentiation and overflows *)
  let newdb = Array.make size 0.0 in

  (* Increase every element *)
  Array.iter (fun row ->
    let idx = elem_to_index row in
    (* Printf.printf "Index: %d\n%!" idx; *)
    newdb.(idx) <- newdb.(idx) +. 1.0
  ) db;

  (* Return the normalized distro *)
  let felem = float_of_int dbi.db_elem in
  Array.map (fun n -> n /. felem
  ) newdb

(* Normalized result for the evalution of a query *)
let eval_query q db = 0.0

let print_db out db =
  (* We don't want the header I guess *)
  (* print_header out db; *)
  Array.iter (Printf.fprintf out "%f ") db
