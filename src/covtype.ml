(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open List
open Str

module Entry = struct
  type covtype_entry = {
    elevation   : int;
    aspect      : int;
    slope       : int;
    horiz_hydro : int;
    vert_hydro  : int;
    horiz_road  : int;
    shade9      : int;
    shade12     : int;
    shade3      : int;
    horiz_fire  : int;
    wild1       : int;
    wild2       : int;
    wild3       : int;
    wild4       : int;
    soil01      : int;
    soil02      : int;
    soil03      : int;
    soil04      : int;
    soil05      : int;
    soil06      : int;
    soil07      : int;
    soil08      : int;
    soil09      : int;
    soil10      : int;
    soil11      : int;
    soil12      : int;
    soil13      : int;
    soil14      : int;
    soil15      : int;
    soil16      : int;
    soil17      : int;
    soil18      : int;
    soil19      : int;
    soil20      : int;
    soil21      : int;
    soil22      : int;
    soil23      : int;
    soil24      : int;
    soil25      : int;
    soil26      : int;
    soil27      : int;
    soil28      : int;
    soil29      : int;
    soil30      : int;
    soil31      : int;
    soil32      : int;
    soil33      : int;
    soil34      : int;
    soil35      : int;
    soil36      : int;
    soil37      : int;
    soil38      : int;
    soil39      : int;
    soil40      : int;
    cover       : int;
  }
end

open Entry

let parse_covtype_entry line =
  let s_line = Str.split (regexp_string ",") line in
  {
    elevation    = int_of_string (nth s_line 0);
    aspect       = int_of_string (nth s_line 1);
    slope        = int_of_string (nth s_line 2);
    horiz_hydro  = int_of_string (nth s_line 3);
    vert_hydro   = int_of_string (nth s_line 4);
    horiz_road   = int_of_string (nth s_line 5);
    shade9       = int_of_string (nth s_line 6);
    shade12      = int_of_string (nth s_line 7);
    shade3       = int_of_string (nth s_line 8);
    horiz_fire   = int_of_string (nth s_line 9);
    wild1        = int_of_string (nth s_line 10);
    wild2        = int_of_string (nth s_line 11);
    wild3        = int_of_string (nth s_line 12);
    wild4        = int_of_string (nth s_line 13);
    soil01       = int_of_string (nth s_line 14);
    soil02       = int_of_string (nth s_line 15);
    soil03       = int_of_string (nth s_line 16);
    soil04       = int_of_string (nth s_line 17);
    soil05       = int_of_string (nth s_line 18);
    soil06       = int_of_string (nth s_line 19);
    soil07       = int_of_string (nth s_line 20);
    soil08       = int_of_string (nth s_line 21);
    soil09       = int_of_string (nth s_line 22);
    soil10       = int_of_string (nth s_line 23);
    soil11       = int_of_string (nth s_line 24);
    soil12       = int_of_string (nth s_line 25);
    soil13       = int_of_string (nth s_line 26);
    soil14       = int_of_string (nth s_line 27);
    soil15       = int_of_string (nth s_line 28);
    soil16       = int_of_string (nth s_line 29);
    soil17       = int_of_string (nth s_line 30);
    soil18       = int_of_string (nth s_line 31);
    soil19       = int_of_string (nth s_line 32);
    soil20       = int_of_string (nth s_line 33);
    soil21       = int_of_string (nth s_line 34);
    soil22       = int_of_string (nth s_line 35);
    soil23       = int_of_string (nth s_line 36);
    soil24       = int_of_string (nth s_line 37);
    soil25       = int_of_string (nth s_line 38);
    soil26       = int_of_string (nth s_line 39);
    soil27       = int_of_string (nth s_line 40);
    soil28       = int_of_string (nth s_line 41); 
    soil29       = int_of_string (nth s_line 42); 
    soil30       = int_of_string (nth s_line 43); 
    soil31       = int_of_string (nth s_line 44); 
    soil32       = int_of_string (nth s_line 45); 
    soil33       = int_of_string (nth s_line 46);  
    soil34       = int_of_string (nth s_line 47); 
    soil35       = int_of_string (nth s_line 48); 
    soil36       = int_of_string (nth s_line 49); 
    soil37       = int_of_string (nth s_line 50); 
    soil38       = int_of_string (nth s_line 51); 
    soil39       = int_of_string (nth s_line 52); 
    soil40       = int_of_string (nth s_line 53); 
    cover        = int_of_string (nth s_line 54); 
  }

let rec parse_covtype_loop ic iter =
  try
    let line = input_line ic in
    (* Printf.printf "%d %!" iter; *)
    (parse_covtype_entry line) ::
      parse_covtype_loop ic (iter + 1)
  with End_of_file ->
    close_in ic; []

let parse_covtype_file file =
  let ic = open_in file in
  parse_covtype_loop ic 0

(*
 * Return a boolean list with n_e elements, all zeros, except for the t_e'th bit
 * which is set to 1. t_e is 0 indexed.
 * *)
let rec gen_bool_list t_e n_e =
  if n_e = 0 then []
  else begin
    (if t_e = 0 then true else false) ::
      gen_bool_list (t_e - 1) (n_e - 1)
  end

(*
 * Turn an integer i, in [min, max], into a bunch of binary attributes. Each
 * attribute covers (size) possible values.
 * *)
let int_to_bin i bot top step =
  let nstep = (top - bot) / step + 1 in
  let dis   = max (min ((i - bot) / step) (nstep - 1)) 0 in
  gen_bool_list dis nstep

let covtype_gen_bin_elem elem =
  let att01 = int_to_bin elem.elevation   0 3999 10        in
  let att02 = int_to_bin elem.aspect      0 360 1          in
  let att03 = int_to_bin elem.slope       0 90 1           in
  let att04 = int_to_bin elem.horiz_hydro (-1000) 1000 100 in
  let att05 = int_to_bin elem.vert_hydro  (-1000) 1000 100 in
  let att06 = int_to_bin elem.horiz_road  (-1000) 1000 100 in
  let att07 = int_to_bin elem.shade9      0 255 20         in
  let att08 = int_to_bin elem.shade12     0 255 20         in
  let att09 = int_to_bin elem.shade3      0 255 20         in
  let att10 = int_to_bin elem.horiz_fire  (-1000) 1000 100 in
  let att11 = int_to_bin elem.wild1       0 1 1            in
  let att12 = int_to_bin elem.wild2       0 1 1            in
  let att13 = int_to_bin elem.wild3       0 1 1            in
  let att14 = int_to_bin elem.wild4       0 1 1            in
  let att15 = int_to_bin elem.soil01      0 1 1            in
  let att16 = int_to_bin elem.soil02      0 1 1            in
  let att17 = int_to_bin elem.soil03      0 1 1            in
  let att18 = int_to_bin elem.soil04      0 1 1            in
  let att19 = int_to_bin elem.soil05      0 1 1            in
  let att20 = int_to_bin elem.soil06      0 1 1            in
  let att21 = int_to_bin elem.soil07      0 1 1            in
  let att22 = int_to_bin elem.soil08      0 1 1            in
  let att23 = int_to_bin elem.soil09      0 1 1            in
  let att24 = int_to_bin elem.soil10      0 1 1            in
  let att25 = int_to_bin elem.soil11      0 1 1            in
  let att26 = int_to_bin elem.soil12      0 1 1            in
  let att27 = int_to_bin elem.soil13      0 1 1            in
  let att28 = int_to_bin elem.soil14      0 1 1            in
  let att29 = int_to_bin elem.soil15      0 1 1            in
  let att30 = int_to_bin elem.soil16      0 1 1            in
  let att31 = int_to_bin elem.soil17      0 1 1            in
  let att32 = int_to_bin elem.soil18      0 1 1            in
  let att33 = int_to_bin elem.soil19      0 1 1            in
  let att34 = int_to_bin elem.soil20      0 1 1            in
  let att35 = int_to_bin elem.soil21      0 1 1            in
  let att36 = int_to_bin elem.soil22      0 1 1            in
  let att37 = int_to_bin elem.soil23      0 1 1            in
  let att38 = int_to_bin elem.soil24      0 1 1            in
  let att39 = int_to_bin elem.soil25      0 1 1            in
  let att40 = int_to_bin elem.soil26      0 1 1            in
  let att41 = int_to_bin elem.soil27      0 1 1            in
  let att42 = int_to_bin elem.soil28      0 1 1            in
  let att43 = int_to_bin elem.soil29      0 1 1            in
  let att44 = int_to_bin elem.soil30      0 1 1            in
  let att45 = int_to_bin elem.soil31      0 1 1            in
  let att46 = int_to_bin elem.soil32      0 1 1            in
  let att47 = int_to_bin elem.soil33      0 1 1            in
  let att48 = int_to_bin elem.soil34      0 1 1            in
  let att49 = int_to_bin elem.soil35      0 1 1            in
  let att50 = int_to_bin elem.soil36      0 1 1            in
  let att51 = int_to_bin elem.soil37      0 1 1            in
  let att52 = int_to_bin elem.soil38      0 1 1            in
  let att53 = int_to_bin elem.soil39      0 1 1            in
  let att54 = int_to_bin elem.soil40      0 1 1            in
  let att55 = int_to_bin elem.cover       0 1 1            in
  Array.of_list (
    att01 @ 
    att02 @ 
    att03 @ 
    att04 @ 
    att05 @ 
    att06 @ 
    att07 @ 
    att08 @ 
    att09 @ 
    att10 @ 
    att11 @ 
    att12 @ 
    att13 @ 
    att14 @ 
    att15 @ 
    att16 @ 
    att17 @ 
    att18 @ 
    att19 @ 
    att20 @ 
    att21 @ 
    att22 @ 
    att23 @ 
    att24 @ 
    att25 @ 
    att26 @ 
    att27 @ 
    att28 @ 
    att29 @ 
    att30 @ 
    att31 @ 
    att32 @ 
    att33 @ 
    att34 @ 
    att35 @ 
    att36 @ 
    att37 @ 
    att38 @ 
    att39 @ 
    att40 @ 
    att41 @ 
    att42 @ 
    att43 @ 
    att44 @ 
    att45 @ 
    att46 @ 
    att47 @ 
    att48 @ 
    att49 @ 
    att50 @ 
    att51 @ 
    att52 @ 
    att53 @ 
    att54 @ 
    att55
  )

let covtype_gen_bin_db file =
  let a_db = parse_covtype_file file        in
  let a_bdb = map covtype_gen_bin_elem a_db in
  Array.of_list a_bdb
