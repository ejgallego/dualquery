(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   Copyright (c) 2015, Mines PARISTECH
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* open Icml *)
open Support
open Query
open Dq
open Cplex

(* open Data *)

module Dq = Dq.Make(MarBQ)
open Dq

(* The module hierachy is a bit wrong here *)
let random_db      : Q.D.db        = Q.D.gen_db 20000 20
let random_queries : Q.query array = Q.gen_n 1000

let ep = {
  exp_eta     = 0.4;
  exp_steps   = 30;
  exp_sample  = 30;
  exp_timeout = 10;
  exp_oracle  = (Oracle.Zero, Oracle.random_oracle 20);
}

  (*   sd_info     : db_info; *)
  (*   sd_queries  : Q.query array; *)
  (*   sd_qcache   : float array; *)
  (* } *)

let ed = {
  sd_info    = Q.D.mk_info "random_test" random_db;
  sd_queries = random_queries;
  sd_qcache  = Q.eval_db_n random_db random_queries;
}

let main () =
  (* Don't forget this! *)
  Random.self_init ();

  let _res = Dq.dq ed ep in
  ()

  (* Call the actual experiment below *)
  (* Param.adult_red () *)
  (* Param.adult_red_params () *)

  (* Simple.ran_bias_mw () *)

  (* Simple.adult_s () *)
  (* Other experiments *)
  (* ML.nf_try () *)
  (* Attributes.ran_bias_ba () *)
  (* Attributes.netflix () *)
  (* Simple.net_s () *)
  (* Simple.netflix () *)
  (* Simple.ran_bias () *)
  (* Param.netflix_z () *)
  (* Param.network () *)
  (* Param.adult () *)
  (* Param.netflix () *)
  (* Queries.network_z () *)
  (* Queries.netflix_z () *)
  (* Param.ran_bias () *)
  (* let _ = Binarray.make 100000 500000 Nativeint.zero in *)
  (* Size.ran_bias_step () *)
  (* Writers.netflix () *)
  (* () *)

let pset p v = p := v

let args_def = [
  "--timeout",         Arg.Int  (pset Params.timeout), "Set CPLEX timeout (in secs)" ;
  "--output",          Arg.Bool (pset Params.output ), "Output the synthetic DB to synthetic.db" ;
]

let parse_args () =
  Arg.parse args_def
     (fun _ -> raise (Arg.Bad "No input needed"))
     "Usage: rundq [options]"


let res =
  parse_args ();
  try main ();
      0
  with Exit x -> x

let () = exit res
