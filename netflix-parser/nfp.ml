(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Convert NetFlix database to a format more suitable for us *)
open Printf
open Str
open Unix

module A = Array
module L = List
module S = String

(* - CustomerIDs range from 1 to 2649429, with gaps. There are 480189 users. *)
let db = A.make (2649429 + 1) ([] : int list)

let add_movie uid mv_id =
  let clist = db.(uid) in
  A.set db uid (mv_id :: clist)

(* Reads an entry, returns the customer id *)
let read_entry line =
  let s_line = Str.split (regexp_string ",") line  in
  let entry  = L.nth s_line 0                      in
  (* eprintf "Line: %s%!" line; *)
  (* eprintf "Entry: %s%!" line; *)
  int_of_string entry

(* Read a movie file *)
let read_movie file =
  let mv_id_str = S.sub file 5 5          in
  let mv_id     = int_of_string mv_id_str in

  (* Dependent on my copy *)
  let file = "training_set/" ^ file       in

  (* Remove useless first line *)
  let ic = open_in file                   in
  let _  = input_line ic                  in
  try
    while true do
      let line  = input_line ic           in
      let uid   = read_entry line         in
      add_movie uid mv_id
    done
  with End_of_file ->
    close_in ic

let read_db () =
  let ts_dir = opendir "training_set" in
  (* Ignore . and .. *)
  let _      = readdir ts_dir in
  let _      = readdir ts_dir in
  let i      = ref 0          in
  try
    while true do
      let next_f = readdir ts_dir in
      eprintf "File: %s [%5d]%!\n" next_f !i;
      i := !i + 1;
      read_movie next_f
    done
  with End_of_file ->
    closedir ts_dir

let rec print_user l = match l with
    []        -> printf "\n"
  | mv :: []  -> printf "%d\n" mv
  | mv :: mvl -> printf "%d,"  mv;
                 print_user mvl

let print_db () =
  A.iter (fun l -> match l with
  | [] -> ()
  | _  -> print_user l
  ) db

let () =
  read_db  ();
  print_db ();
  exit 0
