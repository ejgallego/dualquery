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

let mk_adult_data nqry =
  let (ndb, db_schema, db)    = Data.adult_red                    in
  let db                      = db     ()                         in
  let att_map                 = build_att_map db_schema           in
  let dbinfo                  = mk_db_info ndb db att_map         in
  let qry                     = Q.generate_queries db_schema nqry in
  let qcache                  = Q.eval_queries_norm true db qry   in
  let b_qry                   = Q.to_bin_queries att_map qry      in
  Printf.printf "Adult dataset: Elem: %d, Attr %d, Qrys: %d\n%!" (get_db_elem db) dbinfo.db_bin_att nqry;
  {
    sd_info    = dbinfo;
    sd_queries = b_qry;
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

let all_exp : (exp_data * exp_error * exp_error) list ref = ref []

let append_res data ires res =
  all_exp := (data, ires, res) :: !all_exp

let print_res s data res =
  let open Printf in
  Printf.printf "%s Avg/Max Error for %s: %f/%f\n" s data.sd_info.db_name res.err_avg res.err_max

(* Do an experiment n times and print the results *)
let do_exp n engine data param =
  let open Printf in
  let open Array  in

  (* Initial distribution *)
  let exps = init n (fun idx ->

    printf "Starting experiment run: %d\n" idx;
    (* Init code, we could initialize in a different way *)
    let usize     = Util.pow 2 data.sd_info.db_bin_att     in
    let init      = DbD.uniform_dist usize                 in

    let iqry      = DbD.eval_bqueries init data.sd_queries in
    let ires      = analyze_error data.sd_qcache iqry      in
    print_res "Initial" data ires;

    let dist      = engine data param init                 in
    let rqry      = DbD.eval_bqueries dist data.sd_queries in
    let res       = analyze_error data.sd_qcache rqry      in
    print_res "Final  " data res;
    ires, res
  )                                                        in
  let iexps, exps = map fst exps, map snd exps             in
  let ires, res   = average_exp iexps, average_exp exps    in
  append_res data ires res;
  print_res "Total" data res;
  printf "\n"

let print_all_res () =
  let open Printf in
  printf "\n All exps\n";
  List.iter (fun (d, ir, r) ->
    print_res "Initial" d ir;
    print_res "Final  " d r
  ) !all_exp

(* Actual experiments *)

let do_rbias_exp nelem atts nqry eps t =
  let data  = mk_rbias_data nelem atts nqry       in
  let param = { exp_eps = eps; exp_steps = t; }   in
  do_exp 3 mwem data param
  (* do_exp 3 mw   data param *)

let do_adult_exp times nqry eps t =
  let data  = mk_adult_data nqry                  in
  let param = { exp_eps = eps; exp_steps = t; }   in
  do_exp times mwem data param
  (* do_exp 3 mw   data param *)

let main () =
  (* Don't forget this! *)
  Random.self_init ();

  (* do MW adult_red (avg over 3 runs) with eps from 1 to 5 in 1
     steps, number of queries 5000, steps = 15 *)
  for eps = 1 to 5 do
    do_adult_exp 3 5000 (float_of_int eps) 14
  done;
  print_all_res ()

  (* do_rbias_exp 10000 16 1000 (1.0 12; *)
  (* do_rbias_exp 10000 16 1000 1.0 12; *)

let res =
  try main ();
      0
  with Exit x -> x

let () = exit res
