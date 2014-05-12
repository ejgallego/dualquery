(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* File with helpers to define several types of experiments *)

open Printf

open Db
open Dq
open Epsilon
open Oracle
open Query

open Analytics

module L = Log

(* Result of the experiment *)
type exp_run = (exp_data * exp_param * exp_result * exp_error)

(* Little helper *)
let mk_exp_params eta steps sample oracle =
  {
    exp_eta     = eta;
    exp_sample  = sample;
    exp_steps   = steps;
    exp_timeout = !Params.timeout;
    exp_oracle  = oracle;
  }

let q_of_number i =
  if i < 1000 then
    string_of_int i
  else if i < 1000000 then
    (string_of_int (i / 1000)) ^ "K"
  else
    (string_of_int (i / 1000000)) ^ "." ^ (string_of_int (i / 100000)) ^ "M"

let string_of_exp data param =
  let db   = data.sd_info           in
  let name = db.db_name             in
  let att  = db.db_att              in
  let batt = db.db_bin_att          in
  let elem = q_of_number db.db_elem in
  let nqry = q_of_number (Array.length data.sd_qcache / 2) in

  sprintf "%s-%s[%d\\%d]-q%s-E:%.3f,T:%d,S:%d" name elem att batt nqry
    param.exp_eta param.exp_steps param.exp_sample


let delta = 0.001

module Print = struct (* Output functions *)

  let print_exp_start_data ctx out sd exp =
    let name    = sd.sd_info.db_name                        in
    let att     = sd.sd_info.db_att                         in
    let batt    = sd.sd_info.db_bin_att                     in
    let elem    = sd.sd_info.db_elem                        in
    let eta     = exp.exp_eta                               in
    let steps   = exp.exp_steps                             in
    let sample  = exp.exp_sample                            in
    let qry     = remove_complements sd.sd_queries          in
    let nqry    = Array.length qry                          in
    let epsd    = epsilon_delta delta eta steps sample elem in
    let eps0    = epsilon_0           eta steps sample elem in

    let (ot, orf) = exp.exp_oracle                          in
    fprintf out "Default Oracle: %s\n" (string_of_oracle ot);
    fprintf out "CPLEX Timeout: %d\n" (!Params.timeout);

    let baseline_db  = Array.make 1 (orf ())                             in
    let baseline_res = eval_bqueries_norm false baseline_db qry          in
    let baseline_err = analyze_error ctx nqry sd.sd_qcache baseline_res  in

    fprintf out "Baseline avg: %f; max: %f\n" baseline_err.err_avg baseline_err.err_max;

    fprintf out "| Name: %s | Elements: %d | Att: [%d/%d] | Queries: %d | Eta: %f | Steps: %d | Sample: %d | Eps-d: %f | Eps0: %f |\n%!"
      name elem att batt nqry eta steps sample epsd eps0

  let print_exp_result ctx out header run =
    let (sd, param, res, err) = run           in
    let name    = sd.sd_info.db_name          in
    let att     = sd.sd_info.db_att           in
    let batt    = sd.sd_info.db_bin_att       in
    let elem    = sd.sd_info.db_elem          in
    let eta     = param.exp_eta               in
    let steps   = param.exp_steps             in
    let sample  = param.exp_sample            in

    let qry     = remove_complements sd.sd_queries in
    let nqry    = Array.length qry            in

    let epsd    = epsilon_delta delta eta steps sample elem in
    let eps0    = epsilon_0           eta steps sample elem in

    let (ot, orf)    = param.exp_oracle                                 in
    let oracle_s     = string_of_oracle ot                              in

    let baseline_db  = Array.make 1 (orf ())                            in
    let baseline_res = eval_bqueries_norm false baseline_db qry         in
    let baseline_err = analyze_error ctx nqry sd.sd_qcache baseline_res in

    let h s        = if header then s ^ ": "  else "" in
    let (nz, _, _) = res.res_qd_stats                 in

    (*             name   elem   atts        query  eta    t      s      epsd   eps0   avg    max    to     secs   or     o-avg  o-max  zeros *)
    fprintf out "| %s%s | %s%d | %s[%d/%d] | %s%d | %s%f | %s%d | %s%d | %s%f | %s%f | %s%f | %s%f | %s%d | %s%d | %s%s | %s%f | %s%f | %s%d |\n%!"
      (h "Name")      name
      (h "Elem")      elem
      (h "Att")       att batt
      (h "Queries")   nqry
      (h "Eta")       eta
      (h "T")         steps
      (h "S")         sample
      (h "Eps-d")     epsd
      (h "Eps0")      eps0
      (h "Avg Error") err.err_avg
      (h "Max Error") err.err_max
      (h "TO")        res.res_timeout
      (h "Secs")      res.res_seconds
      (h "Oracle")    oracle_s
      (h "Or-avg")    baseline_err.err_avg
      (h "Or-max")    baseline_err.err_max
      (h "Zeros")     nz;

    if nz > 0 then
      fprintf out "Error: Zeros found in the query distribution!!!!\n"
end

let output_db ctx (_, _, res, _) =
  if !Params.output then
    let out_f = L.open_file ctx "synthethic.db" in
    print_db out_f res.res_db;
    L.close_file out_f
  else
    ()

let res_analysis idx_ctx (exp_data, exp_param, exp_res) =

    (* Here we will get the graphs, etc... *)
    (* Compute error *)
    (* TODO: Must improve this a lot *)

    let qry     = remove_complements exp_data.sd_queries         in
    let nqry    = Array.length qry                               in

    let syn_res = eval_bqueries_norm false exp_res.res_db qry    in

    (* Disable for now         *)
    (* analyze_queries qcache; *)
    let exp_err = analyze_error idx_ctx nqry exp_data.sd_qcache syn_res  in

    let exp = (exp_data, exp_param, exp_res, exp_err)            in

    (* Log every experiment to the general file *)
    Print.print_exp_result idx_ctx (L.tl idx_ctx) true exp;

    (* Write the synthethic db if needed *)
    output_db idx_ctx exp;

    exp

(* Run the same experiment n times, write the results to a log file in
   the current context *)
let do_avg_exp ctx n (exp_data, exp_param) =

  let res_array = Array.init n (fun idx ->

    let idx_dir = sprintf "exp_run-%02d" idx                 in
    let idx_ctx = L.new_dir ctx idx_dir                      in

    let exp_res = dq exp_data exp_param                      in

    res_analysis idx_ctx (exp_data, exp_param, exp_res)
  ) in

  let avg_exp = average_exp res_array                        in
  let avg_out = L.open_file ctx
                (sprintf "avg.txt" )      in
  let all_out = L.open_file ctx
                (sprintf "all.txt" )      in

  Array.iter (fun e -> Print.print_exp_result ctx all_out false e) res_array;

  fprintf all_out "Average:\n";

  Print.print_exp_result ctx all_out    false avg_exp;
  Print.print_exp_result ctx avg_out    false avg_exp;

  (* Log the average to the general file  *)
  Print.print_exp_result ctx (L.tl ctx) true  avg_exp;

  L.close_file all_out;
  L.close_file avg_out

(* Simple experiment *)
let do_exp_single n (exp_data, exp_param) =

  let ctx = L.new_ctx () in

  Print.print_exp_start_data ctx (L.tl ctx) exp_data exp_param;

  (* Run the same experiment n times *)
  do_avg_exp ctx n (exp_data, exp_param)


(* Performs a series of experiments *)
let do_exp_iter n_exp f_exp =

  let ctx  = L.new_ctx ()                                    in

  let _res = Array.init n_exp (fun idx ->

    let (n, exp_data, exp_param) = f_exp idx                 in

    Print.print_exp_start_data ctx (L.tl ctx) exp_data exp_param;

    (* Create the directory for this particular experiment *)
    let exp_dir = sprintf "%02d-%s" idx (string_of_exp exp_data exp_param) in

    let idx_ctx = L.new_dir ctx exp_dir                      in

    do_avg_exp idx_ctx n (exp_data, exp_param)
  ) in
  ()
