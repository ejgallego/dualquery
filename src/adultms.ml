(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Str

let ms_exp = regexp
  "\\([01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01][01]\\)[\t ]+\\([0-9]\\)"

let ms_att  = 27
let ms_elem = 30163

let ms_bool str i =
  if (String.get str i = '1') then true else false

let rec ms_set db idx atts n_entries =
  if n_entries >= 1 then
    begin
      for i = 0 to (ms_att - 1) do
	Array.set db.(idx) i (ms_bool atts i)
      done;
      ms_set db (idx+1) atts (n_entries - 1)
    end
  else
    ()

let ms_split_and_set db s_line idx =
  if Str.string_match ms_exp s_line 0 then
    let (atts, n_entry_s) = (Str.matched_group 1 s_line, Str.matched_group 2 s_line) in
    let n_entry = int_of_string n_entry_s                                            in
    ms_set db idx atts n_entry;
    idx + n_entry
  else
    raise Not_found

let rec read_loop ic db idx =
  try
    let line = input_line ic in
    (* Printf.printf "%s %!" line; *)
    let advance = ms_split_and_set db line idx in
    read_loop ic db advance
  with End_of_file -> close_in ic

let read_msadult file =
  let db = Array.make_matrix ms_elem ms_att false in
  let ic = open_in file                           in
  read_loop ic db 0;
  db
