(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

exception Attribute of string

(* Attribute parsing information *)
type int_att = {
  aint_name : string;
  aint_min  : int;
  aint_max  : int;
  aint_def  : int;
  aint_num  : int;
  (* Cached normalization data *)
  aint_step : float;
  aint_imax : int;
}

type flo_att = {
  aflo_name : string;
  aflo_min  : float;
  aflo_max  : float;
  aflo_def  : float;
  aflo_num  : int;
  (* Cached normalization data *)
  aflo_step : float;
  aflo_imax : int;
}

type bin_att = {
  abin_name : string;
  abin_def  : int;
}

type parse_info =
| ABin    of bin_att
| AInt    of int_att
| AFloat  of flo_att

type att_info = {
  att_name : string;
  att_max  : int;
  att_def  : int;
}

let check_bin_bounds bd ab =
  if not (ab = 0 || ab = 1) then
    raise (Attribute (
      Printf.sprintf "Attribute %s is not binary: %d"
  	bd.abin_name ab
    ))
  else ()

let mk_bin_att name def =
  ABin {
    abin_name = name;
    abin_def  = def;
  }

let mk_int_att name min max def num =
  let ran  = max - min                                 in
  let step = (float_of_int ran) /. (float_of_int num)  in
  let imax = num - 1                                   in
  AInt {
    aint_name = name;
    aint_min  = min;
    aint_max  = max;
    aint_def  = def;
    aint_num  = num;
    (*  *)
    aint_step = step;
    aint_imax = imax;
  }

let check_aint_bounds ad ai =
  if ai < ad.aint_min || ai > ad.aint_max then
    raise (Attribute (
      Printf.sprintf "Attribute %s out of range[%d/%d]: %d"
  	ad.aint_name ad.aint_min ad.aint_max ai
    ))
  else ()

let normalize_int id n =
  check_aint_bounds id n;
  if n = id.aint_max then
    id.aint_imax
  else
    int_of_float (float_of_int n /. id.aint_step)

let mk_float_att name min max def num =
  let ran  = max -. min                 in
  let step = ran /. (float_of_int num)  in
  let j    = ran /. step                in
  let imax = int_of_float j - 1         in
  AFloat {
    aflo_name = name;
    aflo_min  = min;
    aflo_max  = max;
    aflo_def  = def;
    aflo_num  = num;
    (*  *)
    aflo_step = step;
    aflo_imax = imax;
  }

let check_aflo_bounds ad af =
  if af < ad.aflo_min || af > ad.aflo_max then
    raise (Attribute (
      Printf.sprintf "Attribute %s out of range[%f/%f]: %f"
  	ad.aflo_name ad.aflo_min ad.aflo_max af
    ))
  else ()

let normalize_float fd n =
  check_aflo_bounds fd n;
  if n = fd.aflo_max then
    fd.aflo_imax
  else
    int_of_float (n /. fd.aflo_step)

let gen_att_info a = match a with
  | ABin bd   -> {
      att_name = bd.abin_name;
      att_max  = 0;
      att_def  = bd.abin_def;
  }

  | AFloat fd -> {
      att_name = fd.aflo_name;
      att_max  = fd.aflo_imax;
      att_def  = normalize_float fd fd.aflo_def;
    }
  | AInt id -> {
      att_name = id.aint_name;
      att_max  = id.aint_imax;
      att_def  = normalize_int id id.aint_def;
    }

let parse_attribute att str =
  match att with
  | ABin   bd -> let ab = int_of_string str   in
		 check_bin_bounds bd ab;
		 ab

  | AInt   ad -> let ai = int_of_string str   in
		 normalize_int ad ai

  | AFloat ad -> let af = float_of_string str in
		 normalize_float ad af

(* All of the below is not used given that we don't binarize data
   anymore.
*)

(* Binarization *)

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
let att_to_bitlist ad v =
  gen_bool_list v ad.att_max

let float_to_bitlist i bot top step =
  let nstep = int_of_float ((top -. bot) /. step +. 1.0) in
  let dis   = int_of_float ((i -. bot)  /. step)         in
  gen_bool_list dis nstep

(* True binary encoding *)
let rec gen_bin_bool_list elem nbits =
  if nbits = 1 then
    [elem mod 2 = 1]
  else
    (elem mod 2 = 1) ::
      (gen_bin_bool_list (elem / 2) (nbits - 1))

let int_to_bin i bot top step =
  let ftop  = float_of_int top  in
  let fbot  = float_of_int bot  in
  let fstep = float_of_int step in
  let nbits = int_of_float (
    (log ((ftop -. fbot) /. fstep) +. 1.0) /. log 2.0)
  in
  (* Printf.printf "Number of bits for bot, top, step: %d %d %d !! %d\n%!" bot top step nbits; *)
  let elem  = (i - bot) / step  in
  gen_bin_bool_list elem nbits

let float_to_bin i bot top step =
  let nbits = int_of_float ((log ((top -. bot) /. step) +. 1.0) /. log 2.0)
  in
  (* Printf.printf "Number of bits for bot, top, step: %f %f %d !! %d\n%!" bot top step nbits; *)
  let elem  = int_of_float ((i -. bot) /. step)  in
  gen_bin_bool_list elem nbits

(* let discretize_int att = *)
(*   (att.aint_val - att.aint_min) / att.aint_ran *)

(* let discretize_float att = *)
(*   int_of_float ((att.aflo_val -. att.aflo_min) /. att.aflo_ran) *)

(* let discrete_size att = *)
(*   match att with *)
(*   | ABinary _ -> 1 *)
(*   | AInt    a -> ((a.aint_max - a.aint_min) / a.aint_ran) + 1 *)
(*   | AFloat  a -> (int_of_float ((a.aflo_max -. a.aflo_min) /. a.aflo_ran)) + 1 *)

(* The discretization function returns the index (starting on 0) on
   the discrete domain of size discrete_size *)
(* let discretize att = *)
(*   match att with *)
(*   | ABinary b -> if b then 0 else 1 *)
(*   | AInt    a -> discretize_int a *)
(*   | AFloat  a -> discretize_float a *)


