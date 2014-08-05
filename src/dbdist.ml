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

(* Be careful with not reversing the order. *)
let elem_to_index e =
  Array.fold_right (fun b n ->
    (n lsl 1) lor (int_of_bool b)) e 0

let float_of_bool b = if b then 1.0 else 0.0

(* Tests if bit l in idx is 1 *)
let test_lit_pos (idx : int) (l : int) =
  let mask = 1 lsl l in
  (idx land mask) != 0

(* Tests if bit l in idx is 0 *)
let test_lit_neg idx l =
  let mask = 1 lsl l in
  (idx land mask) = 0

(* Eval if a literal is true *)
let ev_lit  idx l = match l with
  | PVar l -> test_lit_pos idx l
  | NVar l -> test_lit_neg idx l

(* Eval the negation of a literal *)
let ev_nlit idx l = match l with
  | PVar l -> test_lit_neg idx l
  | NVar l -> test_lit_pos idx l

(* get l1 /\ l2 /\ l3 *)
let ev_and_blits idx l1 l2 l3 =
  ev_lit  idx l1 && ev_lit  idx l2 && ev_lit  idx l3

let ev_nor_blits idx l1 l2 l3 =
  ev_nlit idx l1 || ev_nlit idx l2 || ev_nlit idx l3

(* Eval a single query on a single element *)
let ev_bquery idx q = float_of_bool (
  match q with
  | BPQuery (l1, l2, l3) -> ev_and_blits idx l1 l2 l3
  | BNQuery (l1, l2, l3) -> ev_nor_blits idx l1 l2 l3
)

(* (normalized) Number of elements that satisfy q *)
let eval_bquery dist q =
  (* EG: We could cache the ev_bquery idx q value, but it wouldn't
     really made a difference *)
  Util.foldi_left (fun idx res prob ->
    (ev_bquery idx q *. prob) +. res  ) 0.0 dist

let eval_bqueries dist ql =
  Array.map (eval_bquery dist) ql

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

let print_db out db =
  (* We don't want the header I guess *)
  (* print_header out db; *)
  Array.iter (Printf.fprintf out "%f ") db
