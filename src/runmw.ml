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


let main () =
  (* Don't forget this! *)
  Random.self_init ();

  (* Call the actual experiment below *)
  (* 20000 elem, 10 atts, 10000 queries *)

  let edata  = mk_rbias_data 30000 12 100             in
  let eparam = { exp_eps = 100.0; exp_steps = 200; }  in
  let res    = mwem edata eparam                      in

  let open Printf in
  let open Array  in
  let open Util   in

  printf "\nReal eval:\n";
  iteri (printf "%d: %f\n") edata.sd_qcache;

  printf "\nNew eval:\n";
  let rqry = DbD.eval_bqueries res edata.sd_queries  in
  iteri (printf "%d: %f\n") rqry;

  let error = mapi (fun i r -> (abs_float (r -. rqry.(i)))) edata.sd_qcache in

  printf "\nError:\n";
  iteri (printf "%d: %f\n") error;

  printf "\nAvg Error: %f\n" (avg error);
  printf "\nAvg Max: %f\n"   (max error);
  ()

let res =
  try main ();
      0
  with Exit x -> x

let () = exit res
