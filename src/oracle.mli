(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

type oracle = unit -> Db.bin_db_row
val zero_oracle   : int -> unit -> bool array
val random_oracle : int -> unit -> bool array

type oracle_type = Zero | Random
val string_of_oracle : oracle_type -> string

val mk_zero_oracle   : int -> oracle_type * (unit -> bool array)
val mk_random_oracle : int -> oracle_type * (unit -> bool array)
