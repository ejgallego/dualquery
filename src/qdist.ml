(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Distributions over queries. implementing alias (numerically stable)
   sampling *)

open Array
open Query

(* Array of the weights of each query *)
type qdist = {
  elem : int;
  dist : float array ; 			(* The actual distribution *)

  (* Alias sampling arrays *)
  prob : float array ;
  alias: int   array ;
}

(* Create a new uniform distribution *)
let qd_new n =
  {
    elem  = n;
    dist  = make n (1.0 /. (float_of_int n));
    prob  = make n 0.0;
    alias = make n 0;
  }

let qd_norm_in_place qd =
  let asum = Util.sum qd.dist in
  Util.map_in_place (fun v -> v /. asum) qd.dist

(* Warning, not private! *)
let qd_update_elem queries query_cache syn_elem eta index qd_elem =
  let query      = queries.(index)                        in
  let q_res      = query_cache.(index)                    in
  let q_res_syn  = eval_bin_elem query syn_elem           in
  let uf         = exp ((-. eta) *. (q_res_syn -. q_res)) in
  (* Printf.printf  "Perfomance of query %d on the synthetic db: %f (%f, %f), update factor %G\n" index (q_res_syn -. q_res) q_res_syn q_res uf; *)
  uf *. qd_elem

(* Warning about precision problems in the distribution *)
let qd_stats qd = Array.fold_left (fun (nz, min, max) -> fun v ->
  let nz'  = if v = 0.0 then nz + 1 else nz               in
  let min' = if v < min then v      else min              in
  let max' = if v > max then v      else max              in
  (nz', min', max')
  ) (0, max_float, min_float) qd.dist

let qd_update_in_place qd queries query_cache syn_elem eta =

  Util.mapi_in_place (qd_update_elem queries query_cache syn_elem eta) qd.dist;
  qd_norm_in_place qd;

  let (nz, min, max) = qd_stats qd in
  Printf.printf "*** (zeros, min, max) after query update: (%d, %G, %G) \n%!" nz min max


(*************************************************************************)
(* Sampling *)

(* A naive sampling apporach doesn't work well for our experiments, we
   frequently have S > 100K, for N >= 1M.

   We implement a numerically stable version of Vose's Alias Method
   (attributed to Walker, Kronmal and Peterson, Vose among others)
   following the description at:

   http://www.keithschwarz.com/darts-dice-coins/

*)

(* TODO, it may be useful to normalize our distribution to N instead
   of 1 like we have been doing now.
*)

let alias_preprocess qd =
  let n = float_of_int qd.elem in

  (* Build the large and small worklists *)
  let (small, large) = Util.foldi_left
    (fun idx (s,l) v ->
      let i_prob = v *. n in
      qd.prob.(idx) <- i_prob;

      if i_prob >= 1.0 then
	(s, idx :: l)
      else
	(idx :: s, l)
    ) ([],[]) qd.dist in

  (* Now build the prob and alias arrays *)
  let rec sm_update (s, l) =
    match (s, l) with
    | [], _          -> (s, l)
    | _, []          -> (s, l)
    | (s::sl, l::ls) ->
      let (ps, pl) = (qd.prob.(s), qd.prob.(l)) in
      let np = (ps +. pl) -. 1.0                in

      qd.alias.(s) <- l;
      (* Done in the list construction *)
      (* qd.prob.(s)  <- ps; *)
      qd.prob.(l)  <- np;

      if np < 1.0 then
	sm_update (l :: sl, ls)
      else
	sm_update (sl, l :: ls)
  in
  let (small, large) = sm_update (small, large) in

  List.iter (fun idx -> qd.prob.(idx) <- 1.0 ) large;
  List.iter (fun idx -> qd.prob.(idx) <- 1.0 ) small;
  if List.length small > 0 then
    Printf.printf "*** WARNING, small list not empty, size: %d\n%!" (List.length small);
  ()

let alias_sample r_idx qd =
  let p     = qd.prob.(r_idx)   in
  let lcoin = Random.float 1.0  in

  if lcoin <= p then
    r_idx
  else
    qd.alias.(r_idx)

let qd_nsample n qd qry =
  alias_preprocess qd;

  (* We can now sample n times *)
  let probs = Array.init n (fun _ -> Random.int qd.elem) in
  Array.map
    (fun rvalue ->
      let q_idx = alias_sample rvalue qd in
      qry.(q_idx)
    ) probs

(* Slow sampling: just for reference *)

let rec qd_sample_slow svalue index qd =
  let cvalue = qd.(index) in
  if svalue <= cvalue then
    index
  else
    qd_sample_slow (svalue -. cvalue) (index + 1) qd

(* Slow sampling *)
let qd_nsample_slow n qd qry =
  let probs = Array.init n (fun _ -> Random.float 1.0) in

  Array.map
    (fun svalue ->
      let q_idx = qd_sample_slow svalue 0 qd in
      qry.(q_idx)
    ) probs

