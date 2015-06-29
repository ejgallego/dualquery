(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Query
open Printf

(* CPlex is quite picky about the input format, so we must "normalize"
   the queries prior to writing them, such that no constant in on the
   left side. Recall that constants on the left side are introduced by
   the negated literals *)

let var_sgn b1 b2 = b1 = b2

let decomp_literal l = match l with
  | PVar i -> (true,  i)
  | NVar i -> (false, i)

let not_int_of_bool b = if b then 0 else 1

(* Return the sign and number of variables, the query factor and the corrected right side *)
let norm_query q = match q with
  | BPQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn true              in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, 3, -correction_factor)

  | BNQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn false             in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, 1, -3+correction_factor)

(****************************** PARITIES *************************************)
(* Return the sign and number of variables, the query factor and the corrected right side *)
let norm_query_p q = match q with
  | BPQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn true              in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, -correction_factor - 1)

  | BNQuery (l1, l2, l3) ->
    let (l1s, l1i) = decomp_literal l1 in
    let (l2s, l2i) = decomp_literal l2 in
    let (l3s, l3i) = decomp_literal l3 in
    let ls = var_sgn false             in
    let correction_factor = (not_int_of_bool l1s + not_int_of_bool l2s + not_int_of_bool l3s) in
    (ls l1s, l1i, ls l2s, l2i, ls l3s, l3i, correction_factor)
(**************************** END PARITIES ***********************************)

let string_of_sgn b = if b then "+" else "-"

let string_of_query qnum q =
  let (l1s, l1i, l2s, l2i, l3s, l3i, qf, rs) = norm_query q in
  sprintf "%s x%d %s x%d %s x%d - %dq%d >= %d"
    (string_of_sgn l1s) l1i
    (string_of_sgn l2s) l2i
    (string_of_sgn l3s) l3i
    qf qnum rs
(****************************** PARITIES *************************************)
let string_of_query_p qnum q =
  let (l1s, l1i, l2s, l2i, l3s, l3i, rs) = norm_query_p q in
  sprintf "%s x%d %s x%d %s x%d - 2p%d - q%d = %d"
    (string_of_sgn l1s) l1i
    (string_of_sgn l2s) l2i
    (string_of_sgn l3s) l3i
    qnum qnum rs
(**************************** END PARITIES ***********************************)

(* Original version for non-negated literals *)

(* let string_of_query qnum q = match q with *)
(*   | PQuery (l1, l2, l3) -> *)
(*     sprintf "%s + %s + %s - 3q%d >= 0" *)
(*                            (string_of_literal l1) *)
(*                            (string_of_literal l2) *)
(*                            (string_of_literal l3) *)
(*                            qnum *)

(*   | NQuery (l1, l2, l3) -> *)
(*     sprintf "-%s -%s -%s - q%d >= -3" *)
(*                            (string_of_literal l1) *)
(*                            (string_of_literal l2) *)
(*                            (string_of_literal l3) *)
(*                            qnum *)

let string_of_queries q = Array.mapi string_of_query q

let string_of_qvar i _ = "q" ^ (string_of_int i)

let string_of_qvars q =
  Array.mapi string_of_qvar q

let string_of_goal q =
  let goal = String.concat "\n + " (Array.to_list (string_of_qvars q)) in
  goal

let rec string_of_atts n =
  if n = 0 then "x0"
  else
    "x" ^ (string_of_int n) ^ "\n " ^ (string_of_atts (n-1))

let string_of_cplex_program file_name q n_att =
  "Problem name: " ^ file_name ^
  "\n\nMaximize\nobj: " ^ (string_of_goal q) ^
  "\n\nSubject To\n" ^ (String.concat "\n" (Array.to_list (string_of_queries q))) ^
  "\n\nBinaries\n" ^ (String.concat "\n " (Array.to_list (string_of_qvars q))) ^
  " " ^ (string_of_atts (n_att -1)) ^
  "\nEnd\n"

let write_cplex file_name q n_att =
  let oc = open_out file_name in
  let contents = string_of_cplex_program file_name q n_att in
  output_string oc contents;
  close_out oc

let print_query out qnum q =
  let (l1s, l1i, l2s, l2i, l3s, l3i, qf, rs) = norm_query q in
  fprintf out "%s x%d %s x%d %s x%d - %dq%d >= %d\n"
    (string_of_sgn l1s) l1i
    (string_of_sgn l2s) l2i
    (string_of_sgn l3s) l3i
    qf qnum rs

(****************************** PARITIES *************************************)
let print_query_p out qnum q =
  let (l1s, l1i, l2s, l2i, l3s, l3i, rs) = norm_query_p q in
  fprintf out "%s x%d %s x%d %s x%d - 2p%d - q%d = %d\n"
    (string_of_sgn l1s) l1i
    (string_of_sgn l2s) l2i
    (string_of_sgn l3s) l3i
    qnum qnum rs
(**************************** END PARITIES ***********************************)

let print_queries out q = Array.iteri (print_query out) q

let print_qvar out i _ =
  fprintf out "q%d\n" i

let print_qvars out q =
  Array.iteri (print_qvar out) q

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
" name print_goal q print_queries q print_qvars q print_atts (n_att - 1)

let print_cplex file_name q n_att =
  let oc = open_out file_name in
  print_program oc file_name q n_att;
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
    true else
    if n = 0
    then false
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

let att_lit l s = match l with
  | PVar v
  | NVar v -> AttSet.add v s

let att_in_query s q = match q with
  | BPQuery (l1, l2, l3)
  | BNQuery (l1, l2, l3) -> att_lit l1 (att_lit l2 (att_lit l3 s))

let atts_in_queries q = Array.fold_left att_in_query AttSet.empty q

let tag_atts q atts =
  let attl = AttSet.elements (atts_in_queries q) in
  List.iter (fun e -> Array.set atts e (-2)) attl

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
  let os = open_out_gen [Open_creat;Open_trunc;Open_text;Open_wronly] 0o644 cplex_cfg_file in
  fprintf os "read %s
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
  atts

open Log

let init_cplex ctx = ()
