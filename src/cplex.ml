(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Oracle
open Query
open Log
open Format

(****************************** PARITIES *************************************)
(**************************** END PARITIES ***********************************)

module type Cplex = sig

  module Q : Qry

  val init_cplex : Log.ctx -> unit
  val run_cplex  : Q.query array -> int -> oracle -> Q.D.db_row

end

module Make (Q : Qry) = struct

module Q = Q

let print_qry_n out q   = Array.iteri (Q.pp_cplex out) q
let print_qvar  out i _ = fprintf out "q%d\n" i
let print_qvars out q   = Array.iteri (print_qvar out) q

let rec print_atts out n =
  if n = 0 then
    fprintf out "x0"
  else
    begin
      fprintf out "x%d\n" n;
      print_atts out (n-1)
    end

let print_goal out q =
  let l = Array.length q - 1 in
  for i = 0 to l - 1 do
    fprintf out " q%d + \n" i
  done;
  if l < 0 then
    fprintf out "\n"
  else
    fprintf out " q%d\n" l

let print_program out name q n_att =
  fprintf out
"Problem name: %s

Maximize
obj:
%a
Subject To
%a
Binaries
%a%a

End
" name print_goal q print_qry_n q print_qvars q print_atts (n_att - 1)

let print_cplex file_name q n_att =
  let oc   = open_out file_name          in
  let ofmt = formatter_of_out_channel oc in
  print_program ofmt file_name q n_att;
  close_out oc

(* TODO *)
let cplex_verbose = false
let cplex_command = false

let cplex_cfg_file = "cplex.cfg"

let call_cplex f_in f_out =
  let _res = Sys.command ("./cplex -f " ^ cplex_cfg_file) in
  ()

(* let cplex_regexp = Str.regexp ".*variable name=.*" *)
let cplex_regexp = Str.regexp ".*variable name=\"x\\([0-9]+\\).*value=\"\\([0-1]\\).*"

(* This should be float *)
exception CPlex of int

let bool_of_int n =
  if n = 1 then
    true
  else if n = 0 then
    false
  else
    raise (CPlex n)

let process_line s_line att_array =
  if Str.string_match cplex_regexp s_line 0 then
    let (a, v) = (Str.matched_group 1 s_line, Str.matched_group 2 s_line) in
    let (an, vn) = (int_of_string a, int_of_string v) in
    Array.set att_array an (bool_of_int vn)
  else
    ()

let rec process_loop ic atts =
  let sol_line = input_line ic in
  process_line sol_line atts;
  process_loop ic atts

let read_cplex file_name atts =
  let ic = open_in file_name in
  try
    process_loop ic atts
  with End_of_file ->
    close_in ic

(* Run cplex returns an array of with the computed attributes for the
   queries *)
module AttSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end )

(*

XXX: Sanity check disabled but would be good to reenable it

let att_lit l s = match l with
  | PVar v
  | NVar v -> AttSet.add v s

let att_in_query s q = match q with
  | BPQuery (l1, l2, l3)
  | BNQuery (l1, l2, l3) -> att_lit l1 (att_lit l2 (att_lit l3 s))

let atts_in_queries q = Array.fold_left att_in_query AttSet.empty q

let tag_atts q atts =
  let attl = AttSet.elements (atts_in_queries q) in
  List.iter (fun e -> atts.(e) <- (-2)) attl
*)

(* WARNING!!!! In some very rare cases, CPlex will return something
   like -4.00000E-30 instead of 0. I don't know how frequent is this,
   but we should perform a sanity check of the result array in the sense that
   no variable remains non-initialized.

   However, other possibility is that the attributes don't appear in
   the queries...
*)

(* TODO: Check that no -2 is in the solution array!! *)
(* Now it is replaced by some other check *)
let sanity_check b = ()

let debug x = printf x; printf "\n%!"

let cplex_cfg_str =
"read %s
set workmem 11000
set mip strategy file 2
set timelimit %d
optimize
write %s
"

let write_cplex_config fin fout timeout =
  let os   = open_out_gen [Open_creat;Open_trunc;Open_text;Open_wronly] 0o644 cplex_cfg_file in
  Printf.fprintf os "read %s
set workmem 11000
set mip strategy file 2
set timelimit %d
optimize
write %s
%!" fin timeout fout;
  close_out os

let run_cplex q n_att oracle =
  let f_in  = "cplex_dq.lp"  in
  let f_out = "cplex_dq.sol" in

  write_cplex_config f_in f_out !Params.timeout;

  if Sys.file_exists f_out then Sys.remove f_out;
  debug "**** Writing CPlex file";
  print_cplex f_in q n_att;
  debug "**** Calling CPlex";
  call_cplex f_in f_out;
  debug "**** CPlex run finished, reading results";

  let atts = oracle () in
  (* Disabled for now we need to polish the conversion more *)
  (* tag_atts q atts; *)

  read_cplex f_out atts;
  debug "**** Results read\n";
  sanity_check atts;

  (* Hack to avoid the type system!!! *)
  Q.D.from_bin atts

let init_cplex ctx = ()

end
