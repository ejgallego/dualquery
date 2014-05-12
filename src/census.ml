(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open List
open Str

module Entry = struct
  type census_entry = {
    age               : int;
    workclass         : int;
    industry_recode   : int;
    occupation_recode : int;
    wage              : int;
    enroll            : int;
    marital           : int;
    industry_code     : int;
    occupation_code   : int;
    race              : int;
    hispanic          : int;
    sex               : int;
    union             : int;
    unemploy          : int;
    employ_status     : int;
    cap_gains         : int;
    cap_losses        : int;
    dividends         : int;
    tax               : int;
    region            : int;
    state             : int;
    house_stat        : int;
    house_summary     : int;
    weight            : int;
    change_msa        : int;
    change_reg        : int;
    change_inreg      : int;
    live_house        : int;
    sunbelt           : int;
    num_employer      : int;
    under18           : int;
    father_birth      : int;
    mother_birth      : int;
    self_birth        : int;
    citizen           : int;
    self_employ       : int;
    vet_admin         : int;
    weeks_worked      : int;
    year              : int;
    income            : int;
  }
end

open Entry

let parse_census_entry line =
  let s_line = Str.split (regexp_string ",") line in
  {
    age               = int_of_string (nth s_line 0);
    workclass         = int_of_string (nth s_line 1);
    industry_recode   = int_of_string (nth s_line 2);
    occupation_recode = int_of_string (nth s_line 3);
    wage              = int_of_string (nth s_line 4);
    enroll            = int_of_string (nth s_line 5);
    marital           = int_of_string (nth s_line 6);
    industry_code     = int_of_string (nth s_line 7);
    occupation_code   = int_of_string (nth s_line 8);
    race              = int_of_string (nth s_line 9);
    hispanic          = int_of_string (nth s_line 10);
    sex               = int_of_string (nth s_line 11);
    union             = int_of_string (nth s_line 12);
    unemploy          = int_of_string (nth s_line 13);
    employ_status     = int_of_string (nth s_line 14);
    cap_gains         = int_of_string (nth s_line 15);
    cap_losses        = int_of_string (nth s_line 16);
    dividends         = int_of_string (nth s_line 17);
    tax               = int_of_string (nth s_line 18);
    region            = int_of_string (nth s_line 19);
    state             = int_of_string (nth s_line 20);
    house_stat        = int_of_string (nth s_line 21);
    house_summary     = int_of_string (nth s_line 22);
    weight            = int_of_string (nth s_line 23);
    change_msa        = int_of_string (nth s_line 24);
    change_reg        = int_of_string (nth s_line 25);
    change_inreg      = int_of_string (nth s_line 26);
    live_house        = int_of_string (nth s_line 27);
    sunbelt           = int_of_string (nth s_line 28);
    num_employer      = int_of_string (nth s_line 29);
    under18           = int_of_string (nth s_line 30);
    father_birth      = int_of_string (nth s_line 31);
    mother_birth      = int_of_string (nth s_line 32);
    self_birth        = int_of_string (nth s_line 33);
    citizen           = int_of_string (nth s_line 34);
    self_employ       = int_of_string (nth s_line 35);
    vet_admin         = int_of_string (nth s_line 36);
    weeks_worked      = int_of_string (nth s_line 37);
    year              = int_of_string (nth s_line 38);
    income            = int_of_string (nth s_line 39);
  }

let rec parse_census_loop ic iter =
  try
    let line = input_line ic in
    (* Printf.printf "%d %!" iter; *)
    (parse_census_entry line) ::
      parse_census_loop ic (iter + 1)
  with End_of_file ->
    close_in ic; []

let parse_census_file file =
  let ic = open_in file in
  parse_census_loop ic 0

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

let census_gen_bin_elem elem =
  let att01 = int_to_bin elem.age               0 99 10      in
  let att02 = int_to_bin elem.workclass         0 9 1        in
  let att03 = int_to_bin elem.industry_recode   0 52 1       in
  let att04 = int_to_bin elem.occupation_recode 0 47 1       in
  let att05 = int_to_bin elem.wage              0 999 10     in
  let att06 = int_to_bin elem.enroll            0 3 1        in
  let att07 = int_to_bin elem.marital           0 7 1        in
  let att08 = int_to_bin elem.industry_code     0 24 1       in
  let att09 = int_to_bin elem.occupation_code   0 15 1       in
  let att10 = int_to_bin elem.race              0 5 1        in
  let att11 = int_to_bin elem.hispanic          0 10 1       in
  let att12 = int_to_bin elem.sex               0 2 1        in
  let att13 = int_to_bin elem.union             0 3 1        in
  let att14 = int_to_bin elem.unemploy          0 6 1        in
  let att15 = int_to_bin elem.employ_status     0 8 1        in
  let att16 = int_to_bin elem.cap_gains         0 9999 100   in
  let att17 = int_to_bin elem.cap_losses        0 9999 100   in
  let att18 = int_to_bin elem.dividends         0 9999 100   in
  let att19 = int_to_bin elem.tax               0 6 1        in
  let att20 = int_to_bin elem.region            0 6 1        in
  let att21 = int_to_bin elem.state             0 50 1       in
  let att22 = int_to_bin elem.house_stat        0 38 1       in
  let att23 = int_to_bin elem.house_summary     0 8 1        in
  let att24 = int_to_bin elem.weight            0 9999 1000  in
  let att25 = int_to_bin elem.change_msa        0 9 1        in
  let att26 = int_to_bin elem.change_reg        0 8 1        in
  let att27 = int_to_bin elem.change_inreg      0 9 1        in
  let att28 = int_to_bin elem.live_house        0 3 1        in
  let att29 = int_to_bin elem.sunbelt           0 3 1        in
  let att30 = int_to_bin elem.num_employer      0 499 100    in
  let att31 = int_to_bin elem.under18           0 5 1        in
  let att32 = int_to_bin elem.father_birth      0 42 1       in
  let att33 = int_to_bin elem.mother_birth      0 42 1       in
  let att34 = int_to_bin elem.self_birth        0 42 1       in
  let att35 = int_to_bin elem.citizen           0 5 1        in
  let att36 = int_to_bin elem.self_employ       0 3 1        in
  let att37 = int_to_bin elem.vet_admin         0 3 1        in
  let att38 = int_to_bin elem.weeks_worked      0 59 1       in
  let att39 = int_to_bin elem.year              0 2 1        in
  let att40 = int_to_bin elem.income            0 2 1        in
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
    att40
  )

let census_gen_bin_db file =
  let a_db = parse_census_file file        in
  let a_bdb = map census_gen_bin_elem a_db in
  Array.of_list a_bdb
