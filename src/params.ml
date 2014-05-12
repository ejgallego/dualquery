(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Common params *)

(* Number of cores for paralell query evaluation *)
let n_cores = 4

(* Parameters *)
let timeout :  int ref = ref 30
let output  : bool ref = ref false
