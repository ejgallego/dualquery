(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Module for analytics *)

open Printf
open Util

module L = Log

(* Database analytics *)

(* TODO, here we can generate plots about the data, the queries, etc... *)

(* Information about the error in a dq run *)
type exp_error = {
  err_avg     : float;
  err_max     : float;
  err_rel_avg : float;
}

(********************************************************************* *)
(* Histogram helper functions                                          *)
(* Takes an array of floats and returns an histogram                   *)
let array_to_histo (arr : float array) n_buckets =
  let min         = min arr                                  in
  let max         = max arr                                  in
  let res         = Array.make (n_buckets + 1) 0             in
  let bucket_size = (max -. min) /. (float_of_int n_buckets) in
  Array.iter (fun item ->
    let bucket = int_of_float ((item -. min) /. bucket_size) in
    Array.set res bucket ((Array.get res bucket) + 1)
  ) arr;
  (bucket_size, min, res)

(* Print histogram in GnuPlot format *)
(* set style data linespoints
   set terminal png
   set output "error.png"
   plot 'error.dat'
 *)
let print_histo (b_s, min, arr) out =
  Array.iteri (fun bucket -> fprintf out "%8f %8d\n" ((b_s *. (float_of_int bucket)) +. min)) arr

let with_outfile ctx file f =
  let out = L.open_file ctx file in
  f out;
  L.close_file out

(********************************************************************* *)
(* Query and db analysis functions                                     *)

let analyze_queries ctx qresult =
  let qry_histo = array_to_histo qresult 50                      in
  with_outfile ctx "qresult.dat" (print_histo qry_histo)

(********************************************************************* *)
(* Error analysis functions                                            *)

let rel_error real_v syn =
  (abs_float (real_v -. syn)) /. real_v

let analyze_error ctx nqry real_db syn_db =
  (* Absolute error per query *)
  let abs_q_err = Array.init nqry (fun idx ->
    let v1 = Array.get real_db idx in
    let v2 = Array.get syn_db idx  in
    abs_float (v1 -. v2)           )                             in

  (* Total additive error *)
  let t_err   = sum abs_q_err               in

  (* Maximum error *)
  let max_err = max abs_q_err                in

  (* Average error *)
  let a_err   = t_err /. (float_of_int nqry)                     in

  (* Print error histogram *)
  let err_histo = array_to_histo abs_q_err 50                    in
  with_outfile ctx "error.dat" (print_histo err_histo);

  (* Relative error *)
  let rel_q_err = Array.init nqry (fun idx ->
    let q_real = real_db.(idx) in
    let q_syn  = syn_db.(idx)  in
    if (q_real > 0.0) then
      rel_error q_real q_syn
    else
      -1.0)                            in
  let t_rel_err   = sum rel_q_err                                in
  let a_rel_err   = t_rel_err /. (float_of_int nqry)             in

  (* Print rerror histogram *)
  let rel_histo = array_to_histo rel_q_err 1500                  in
  with_outfile ctx "rerror.dat" (print_histo rel_histo);

  {
    err_avg = a_err;
    err_max = max_err;
    err_rel_avg = a_rel_err
  }

open Dq

module type Analytics = sig

module E : Dq

val average_exp   : (E.exp_data * exp_param * E.exp_result * exp_error) array ->
                    (E.exp_data * exp_param * E.exp_result * exp_error)

end

module Make(DQ : Dq) = struct

  module E = DQ

let average_exp e_arr =
  let n_elems = float_of_int (Array.length e_arr) in
  let sum_max = Array.fold_left (fun n (_, _, _, e) -> n +. e.err_max) 0.0 e_arr in
  let sum_avg = Array.fold_left (fun n (_, _, _, e) -> n +. e.err_avg) 0.0 e_arr in
  let n_err =
    {
      err_avg = sum_avg /. n_elems;
      err_max = sum_max /. n_elems;
      err_rel_avg = -1.0
    } in
  let (a, b, c, _) = Array.get e_arr 0 in
  (a, b, c, n_err)

end
