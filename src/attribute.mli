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

(* 0 is always the minimum *)
type att_info = {
  att_name : string;
  att_max  : int;
  att_def  : int;
}

val mk_bin_att      : string ->                            int -> parse_info
val mk_int_att      : string -> int   -> int   -> int   -> int -> parse_info
val mk_float_att    : string -> float -> float -> float -> int -> parse_info

val gen_att_info    : parse_info -> att_info

val parse_attribute : parse_info -> string -> int

val att_to_bitlist  : att_info -> int -> bool list

