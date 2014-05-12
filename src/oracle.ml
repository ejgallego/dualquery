(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

type oracle_type =
  | Zero
  | Random

let string_of_oracle o = match o with
  | Zero   -> "zero"
  | Random -> "random"

type oracle = unit -> Db.bin_db_row

(* An oracle returns a element of the database  *)
let random_oracle att () =
  Array.init att (fun _ -> Random.bool ())

let zero_oracle   att () =
  Array.make att false

let mk_random_oracle att =
  (Random, random_oracle att)

let mk_zero_oracle   att =
  (Zero, zero_oracle att)
