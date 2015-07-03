(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* open Icml *)
open Support
(* open Query *)
(* open Data *)
open Dq

let main () =

  ()
  (* Don't forget this! *)
  (* Random.self_init (); *)

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
