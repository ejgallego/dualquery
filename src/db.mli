(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Array

open Attribute

(* A db schema is a list of attributes *)
type db_schema = att_info array
type att_map   = (int * int) array

(* Information about the database *)
type db_info = {
  db_name    : string;
  db_att     : int;
  db_bin_att : int;
  db_elem    : int;
}

(* A database is a MxN matrix of attributes for now, an attribute is
   just an integer. How to interpret it depends on the schema.
*)
type db_row = int array
type db     = db_row array

(* Binary database *)
type bin_db_row = bool array
type bin_db     = bin_db_row array

val build_att_map : db_schema -> att_map

val get_db_att  : db -> int
val get_db_elem : db -> int

val get_bin_db_att  : bin_db -> int
val get_bin_db_elem : bin_db -> int

val generate_bin_db      : int -> int -> bin_db
val generate_bin_db_bias : int -> int -> bin_db

val mk_db_info : string -> db -> att_map -> db_info

val print_db : out_channel -> bin_db -> unit
