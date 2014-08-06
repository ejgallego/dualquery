(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Util

let epsilon_0 eta t s n =
  let t = float_of_int t in
  let s = float_of_int s in
  let n = float_of_int n in

  eta *. t *. (t -. 1.0) *. s /. n

(* Compute the sample given a target epsilon *)
let calc_sample_eps0 eps eta t n =
  let t = float_of_int t                              in
  let n = float_of_int n                              in

  let f_sample = eps *. n /. (eta *. t *. (t -. 1.0)) in
  Util.best_int_of_float f_sample

let epsilon_delta_core delta eta t s n =
  let g  = 2.0 *. eta /. n      in
  let m  = s *. t *. (t -. 1.0) in
  let e1 = exp (g *. t)         in
  let e2 = exp g                in
  let l  = -. ( (e1 -. e2) /. ((e2 -. 1.0) *. (e2 -. 1.0)) ) +.
  		(t -. 1.0) *. e1 /. (e2 -. 1.0) in

  (* let k = sqrt (log (1.0 /. delta) *. m *. (2.0 *. t -. 1.0) /. 3.0) in *)
  (* Printf.printf "g: %.20f\nm: %.20f\ne1: %.20f\ne2: %.20f\nl: %.20f\nk: %.20f\nm/2: %.20f\ns*l: %.20f\n" *)
  (*   g m e1 e2 l k (m/.2.0) (s *. l); *)

  g *. (sqrt (log (1.0 /. delta) *. m *. (2.0 *. t -. 1.0) /. 3.0) -.
  	  m /. 2.0 +.
	  s *. l)

(* Laplace simulations for eps_0 *)
let lap sigma =
  let unif = (Random.float 1.0) -. 0.5 in
  let s = if unif < 0.0 then -1.0 else 1.0 in
    s *. sigma *. log(1.0 -. 2.0 *. (abs_float unif));;

let lap_e0_avg_error n k =
  let k' = float_of_int k in
  (sum (Array.init k (fun _ -> abs_float (lap k'))) /. k') /. n

let lap_e0_max_error n k =
  let k' = float_of_int k in
  (max (Array.init k (fun _ -> abs_float (lap k')))) /. 500000.0

(* Laplace simulations for eps_delta *)


let epsilon_delta delta eta t s n =
  let t = float_of_int t       in
  let s = float_of_int s       in
  let n = float_of_int n       in

  epsilon_delta_core delta eta t s n

let epsilon_delta_b2_core delta eta t s n =
  let k = s *. (t -. 1.0)      in

  let e' = 2.0 *. eta *. (t -. 1.0) /. n in

  e' *. (
    sqrt (2.0 *. k *. log (1.0 /. delta)) +.
    k *. ( exp (e') -. 1.0 )
  )

let epsilon_delta_b2 delta eta t s n =
  let t = float_of_int t       in
  let s = float_of_int s       in
  let n = float_of_int n       in

  epsilon_delta_b2_core delta eta t s n

(* Compute the sample given a target epsilon. Use a simple binary
   search, provided f_opt is monotone in its parameter. *)
let rec binary_search objective b_min b_max err f_opt =
  let new_v = b_min +. (b_max -. b_min) /. 2.0 in
  let new_r = f_opt new_v                      in

  if abs_float (f_opt new_v -. objective) < err then
    new_v
  else (
    if new_r > objective then
      binary_search objective b_min new_v err f_opt
    else
      binary_search objective new_v b_max err f_opt
    )

let max_sample = 10000000.0

let calc_sample_eps_delta delta eps eta t n =
  let t   = float_of_int t                                 in
  let n   = float_of_int n                                 in
  let f s = epsilon_delta_core delta eta t s n             in

  (* FIXME: Improve the 100000.0 stuff *)
  let f_sol = binary_search eps 0.0 max_sample 0.001 f      in
  Util.best_int_of_float f_sol


(* Compute the number of steps given a target epsilon. *)
let calc_steps_eps_delta delta eps eta s n =
  let s   = float_of_int s                                 in
  let n   = float_of_int n                                 in
  let f t = epsilon_delta_core delta eta t s n             in
  let f_sol = binary_search eps 0.0 10000.0 0.001 f        in
  Util.best_int_of_float f_sol


(* Compute the number of steps given a target epsilon. *)
let calc_eta_eps_delta delta eps step s n =
  let step = float_of_int step                              in
  let s    = float_of_int s                                 in
  let n    = float_of_int n                                 in
  let f eta  = epsilon_delta_core delta eta step s n        in
  binary_search eps 0.0 20.0 0.0001 f

let calc_sample_eps_delta_b2 delta eps eta t n =
  let t   = float_of_int t                                 in
  let n   = float_of_int n                                 in
  let f s = epsilon_delta_b2_core delta eta t s n          in

  (* FIXME: Improve the 100000.0 stuff *)
  let f_sol = binary_search eps 0.0 max_sample 0.001 f      in
  Util.best_int_of_float f_sol

let epsilon_delta_core_b1 delta eta t s n =
  2.0 *. eta *. t *. sqrt (8.0 *. t *. s *. (log (1.0 /. delta))) /. n

let epsilon_delta_b1 delta eta t s n =
  let t  = float_of_int t in
  let s  = float_of_int s in
  let n  = float_of_int n in

  epsilon_delta_core_b1 delta eta t s n

let calc_sample_eps_delta_b1 delta eps eta t n =
  let t   = float_of_int t                                 in
  let n   = float_of_int n                                 in
  let f s = epsilon_delta_core_b1 delta eta t s n          in

  (* FIXME: Improve the 100000.0 stuff *)
  let f_sol = binary_search eps 0.0 max_sample 0.001 f       in
  Util.best_int_of_float f_sol

(* Old bounds, the last one is wrong, kept it for documentation
   purposes *)

let epsilon_delta_all delta eta t s n =
  (epsilon_delta_b1 delta eta t s n,
   epsilon_delta_b2 delta eta t s n,
   epsilon_delta    delta eta t s n)

let calc_sample_all delta eps eta t n =
  (calc_sample_eps_delta_b1 delta eps eta t n,
   calc_sample_eps_delta_b2 delta eps eta t n,
   calc_sample_eps_delta    delta eps eta t n)

(* Just for reference wrt old experiments. *)
let epsilon_delta_wrong delta eta t s n =
  let steps  = float_of_int t in
  let sample = float_of_int s in
  let elem   = float_of_int n in

  eta *. steps *. sqrt (2.0 *. sample *. steps *. (log (1.0 /. delta))) /.
    elem

(* [Old] Code to compute s and T from α and β                              *)
(*   let eta    = alpha /. 4.0                             in              *)
(*   let step   = (16.0 *. (log (float_of_int n_qry))) /.                  *)
(*                (alpha *. alpha)                         in              *)
(*   let sample =                                                          *)
(*     let lp = (2.0 *. (2.0 ** (float_of_int atts)) *. step) /. beta      *)
(*     in       (2.0 *. log lp) /. (alpha *. alpha)        in              *)
