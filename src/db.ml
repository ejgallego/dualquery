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

end

module BinDb = struct

  type t = bool

  type db_row = bool   array
  type db     = db_row array

  let mk_info name db = {
    db_name     = name;
    db_att      = Array.length db.(0);
    db_bin_att  = Array.length db.(0);
    db_elem     = Array.length db;
  }

  let gen = Random.bool

end

module IntDb = struct

  type t = int

  type db_row = int   array
  type db     = db_row array

  let mk_info name db = {
    db_name     = name;
    db_att      = Array.length db.(0);
    db_bin_att  = Array.length db.(0);
    db_elem     = Array.length db;
  }

  let gen () = Random.int 20
end

(* module IntDb : (Db with type t = int) *)

(*

*)
