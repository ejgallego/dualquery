(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Db
open Query

type bin_row = bool array

module BinDb = struct

  type db_row = bool    array
  type db     = bin_row array

  let mk_db_info db name = {
    db_name     = name;
    db_att      = Array.length db.(0);
    db_bin_att  = Array.length db.(0);
    db_elem     = Array.length db;
  }

  let get_row db i = db.(i)
  let fold_rows = Array.fold_left

end



