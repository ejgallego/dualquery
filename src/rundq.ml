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
(* Porting data is still todo... I have a stash with quite a bit of progress *)

(* Marginal *)
module DQM = Dq.Make(MarBQ)
module EM  = Exp.Make(DQM)

(* Parities *)
module DQP = Dq.Make(ParBQ)
module EP  = Exp.Make(DQP)

let ep = {
  exp_eta     = 0.8;
  exp_steps   = 100;
  exp_sample  = 90;
  exp_timeout = 20;
  exp_oracle  = (Oracle.Random, Oracle.random_oracle 20);
}

(* XXX: We should be able to share the paramteres as the db is the
   same, but the module hierachy is a bit wrong here *)
let mar_exp () =

  let open DQM                                             in
  let random_db      : Q.D.db        = Q.D.gen_db 20000 20 in
  let random_queries : Q.query array = Q.gen_n 1000 20     in
  let ed =
    let info = Q.D.mk_info "random_par" random_db     in
    let nqry = Q.neg_n random_queries                 in
    {
      sd_info    = info;
      sd_queries = nqry;
      sd_qcache  = Q.eval_db_n random_db nqry;
    }
  in
  EM.do_exp_single 2 (ed, ep)

let par_exp () =

  let open DQP                                             in
  let random_db      : Q.D.db        = Q.D.gen_db 20000 20 in
  let random_queries : Q.query array = Q.gen_n 1000 20     in
  let ed =
    let info = Q.D.mk_info "random_par" random_db     in
    let nqry = Q.neg_n random_queries                 in
    {
      sd_info    = info;
      sd_queries = nqry;
      sd_qcache  = Q.eval_db_n random_db nqry;
    }
  in
  EP.do_exp_single 2 (ed, ep)

let main () =
  (* Don't forget this! *)
  Random.self_init ();

  (* Format.printf "qcache: "; *)
  (* Array.iter (fun f -> Format.printf "%f " f) ed.sd_qcache; *)

  (* Format.printf "qry: "; *)
  (* Array.iter (fun q -> Format.printf "%s\n" (Q.to_string q)) ed.sd_queries; *)

  mar_exp ();
  par_exp ();
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
