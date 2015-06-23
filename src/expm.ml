(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Dbdist
open Util

(* Multiplicative weights update rule *)

(* Sampling from an array. Due to unboundness of the sum of the input
   distribution we have to compute it *)

let rec d_sample_slow svalue index d =
  let cvalue = d.(index)   in
  if svalue <= cvalue then
    index
  else
    d_sample_slow (svalue -. cvalue) (index + 1) d

let d_sample d k =
  let sum  = sum d            in
  let prob = Random.float sum in

(*
  Debug code:
*)
  let open Array  in
  let open Printf in

  let (d' : (int * float * float) array) = mapi (fun idx d -> (idx, d, k.(idx))) d in
  let comp (x : (int * float * float)) (y : (int * float * float)) =
    let (_, x, _), (_, y, _) = x, y in
    - (compare x y)
  in
  sort comp d';
  let d' = sub d' 0 3 in
  (* printf "Sum of EXPM: %f, RanVal: %f\n%!" sum prob; *)
  printf "Best candidates:\n%!";
  printf "-----------------------\n%!";
  iter (fun (q, v, d) -> printf "q%03d: %f - %f\n%!" q v d) d';
  printf "-----------------------\n%!";

  d_sample_slow prob 0 d

(* TODO: Verify the score function *)
let exp_mech eps n score =
  let e2 = eps /. 2.0         in
  let sa = Array.init n score in

  let sa_max = max sa         in
  let k = Array.copy sa       in
  map_in_place (fun v -> exp ((v -. sa_max) *. e2 )) sa;
  (* The sum is guaranteed to be one? *)
  d_sample sa k

(* Issues with precision here *)
(* let exp_mech eps n score = *)
(*   let sa = Array.init n (fun idx -> exp (eps *. score idx /. 2.0)) in *)
(*   d_sample sa *)

