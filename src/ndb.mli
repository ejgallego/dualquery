(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Array
open Attribute

(* A db schema is a list of attributes *)
type db_schema = att_info array

(* A map of regular to binary attributes, (start, size) *)
type att_map   = (int * int)  array

(* Information about the database *)
type db_info = {
  db_name    : string;
  db_att     : int;
  db_bin_att : int;
  db_elem    : int;
}

module type Db = sig

  type db_row
  type db

  val make_db_info : db -> db_info
  val get_row : db -> int -> db_row

  val fold_rows : ('a -> db_row -> 'a) -> 'a -> db -> 'a

end
