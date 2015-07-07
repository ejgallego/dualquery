(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Array
open Attribute

(* A db schema is a list of attributes *)
type db_schema = att_info array

(* A map of regular to binary attributes, (start, size) *)
(* type att_map   = (int * int) array *)

(* Information about the database *)
type db_info = {
  db_name    : string;
  db_att     : int;
  db_bin_att : int;
  db_elem    : int;
}

(* Binary databases *)
module type Db = sig

  type t

  type db_row = t      array
  type db     = db_row array

  val mk_info : string -> db -> db_info

  (* Random generation *)
  val gen     : unit -> t
  val gen_db  : int -> int -> db

  val from_bin : bool array -> db_row
end

module BinDb : Db with type t = bool
module IntDb : Db with type t = int

(* vs *)
(* module BinDb : (Db with type t := bool) *)

(*
module type Db = sig

  type t
  type schema = att_info

  type db_row

  To generalize to nfArray we'd need to add row and db type, for
  now we assume db's to be "t array array"

  type db_row
  type db

  val make_row   : int -> (int -> db_elem) -> db_row
  val get_row    : db -> int -> db_row
  val fold_rows  : ('a -> db_row -> 'a) -> 'a -> db -> 'a
end
*)


(*

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
 *)
