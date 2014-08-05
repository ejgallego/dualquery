(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Dbdist

(* Multiplicative weights update rule *)

(* Sampling from an array. Due to unboundness of the sum of the input
   distribution we have to compute it *)

let d_sum d = Array.fold_left (+.) 0.0 d

let rec d_sample_slow svalue index d =
  let cvalue = d.(index)   in
  if svalue <= cvalue then
    index
  else
    d_sample_slow (svalue -. cvalue) (index + 1) d

let d_sample d =
  let sum  = d_sum d          in
  (* Printf.printf "Sum of MWEM: %f\n%!" sum; *)

  let prob = Random.float sum in
  d_sample_slow prob 0 d

(* TODO: Verify the score function *)
let exp_mech eps n score =
  let score_array = Array.init n (fun idx -> exp (eps *. score idx /. 2.0)) in
  d_sample score_array

