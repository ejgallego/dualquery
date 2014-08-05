(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Support

(* module D = Data *)
module DbD = Dbdist
module Q = Query

let mk_rbias_dist elem atts nqry =
  let db     = generate_bin_db_bias atts elem     in
  let dbinfo = { db_name    = "ran_bias";
                 db_att     = atts;
                 db_bin_att = atts;
                 db_elem    = elem; }             in
  let udist  = DbD.to_dist dbinfo db              in

  let qry     = Q.generate_bqueries  atts nqry    in
  let qcache  = Q.eval_bqueries_norm true db qry  in
  let qdcache = DbD.eval_bqueries udist qry       in

  let open Printf in
  (* Compare the result *)
  printf "Old eval:\n";
  Array.iter (printf "%f\n") qcache;
  printf "\n\nNew eval:\n";
  Array.iter (printf "%f\n") qdcache;

  udist

    (* let c_qry    = Q.complement_bqueries qry               in *)
    (* let c_qcache = Q.complement_qcache   qcache            in *)
    (* { *)
    (*   sd_queries = c_qry; *)
    (*   sd_qcache  = c_qcache; *)
    (* } *)


let main () =
  (* Don't forget this! *)
  Random.self_init ();

  (* Call the actual experiment below *)
  (* 20000 elem, 10 atts, 10000 queries *)

  let udist = mk_rbias_dist 20000 5 20 in

  (* print_db stdout udist; *)
  ()

let res =
  try main ();
      0
  with Exit x -> x

let () = exit res
