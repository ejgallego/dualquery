(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Support
open Mwem

(* module D = Data *)
module DbD = Dbdist
module Q = Query

let mk_rbias_data elem atts nqry =
  let db     = generate_bin_db_bias atts elem     in
  let dbinfo = { db_name    = "ran_bias";
                 db_att     = atts;
                 db_bin_att = atts;
                 db_elem    = elem; }             in

  (* let udist  = DbD.to_dist dbinfo db              in *)
  (* let qdcache = DbD.eval_bqueries udist qry       in *)
  (* let open Printf in *)
  (* (\* Compare the result *\) *)
  (* printf "Old eval:\n"; *)
  (* Array.iter (printf "%f\n") qcache; *)
  (* printf "\n\nNew eval:\n"; *)
  (* Array.iter (printf "%f\n") qdcache; *)

  let qry     = Q.generate_bqueries  atts nqry    in
  let qcache  = Q.eval_bqueries_norm true db qry  in


  { sd_info    = dbinfo;
    sd_queries = qry;
    sd_qcache  = qcache;
  }

(* Information about the error in a run *)
type exp_error = {
  err_avg     : float;
  err_max     : float;
}

let analyze_error rqry sqry =
  let open Array  in
  let open Util   in

  (* Error per query *)
  let error = mapi (fun i r -> (abs_float (r -. sqry.(i)))) rqry in
  (* printf "\nError:\n"; *)
  (* iteri (printf "%d: %f\n") error; *)
  {
    err_avg = avg error;
    err_max = max error;
  }

let average_exp (e_arr : exp_error array) =
  let n_elems = float_of_int (Array.length e_arr)                     in
  let sum_max = Array.fold_left (fun n e -> n +. e.err_max) 0.0 e_arr in
  let sum_avg = Array.fold_left (fun n e -> n +. e.err_avg) 0.0 e_arr in
  {
    err_avg = sum_avg /. n_elems;
    err_max = sum_max /. n_elems;
  }

(* Do an experiment n times and print the results *)
let do_exp n data param =

  let open Printf in

  let exps = Array.init n (fun idx ->
    let dist = mwem data param                        in
    let rqry = DbD.eval_bqueries dist data.sd_queries in
    let res  = analyze_error data.sd_qcache rqry      in
    printf "\nAvg/Max Error: %f/%f\n" res.err_avg res.err_max;
    res
  ) in
  let res = average_exp exps in
  printf "\nAvg/Max Error: %f/%f\n" res.err_avg res.err_max

let do_rbias_exp () =
  let data  = mk_rbias_data 32000 18 1000          in
  let param = { exp_eps = 1.0; exp_steps = 10; }   in
  do_exp 2 data param

let main () =
  (* Don't forget this! *)
  Random.self_init ();

  do_rbias_exp ()

let res =
  try main ();
      0
  with Exit x -> x

let () = exit res
