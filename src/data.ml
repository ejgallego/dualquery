(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Dq
open Query

open Printf

type db_gen       = unit -> db
type data_source  = (string * db_schema * db_gen)
type query_source = db_schema -> (string * query array)

(* Data sources *)
let network     = ("network",   Network.schema,   Network.read_db   "data/kdd99.clean")
let network_s   = ("network",   Network.schema,   Network.read_db   "data/kdd99-small.clean")

let adult       = ("adult",     Adult.schema,     Adult.read_db     "data/adult.clean")

let tictactoe   = ("tictactoe", Tictactoe.schema, Tictactoe.read_db "data/tic-tac-toe.data")
let ttt_bin     = ("tt_bin",    Ttt_bin.schema,   Ttt_bin.read_db   "data/tic-tac-toe.data")

(* Superceeded by more efficient implementation *)
(* let random   att el () = ("random",      generate_bin_db att el) *)
(* let ran_bias att el () = ("r_bias",      generate_bin_db_bias att el) *)

(* Should convert to new schema format *)
(* let msadult       () = ("msadult",   Adultms.read_msadult       "data/mcsherry-adult.txt") *)
(* let census        () = ("census",    Census.census_gen_bin_db   "data/census-income.clean") *)
(* let covtype       () = ("covtyp",    Covtype.covtype_gen_bin_db "data/covtype.clean") *)

(* Generate random queries *)
let ran_queries n db_schema = ("q" ^ (string_of_int n), generate_queries db_schema n)

(* let all       db = ("all",                      generate_all_queries (get_db_att db)) *)
(* let lin.... *)

(* Query and database helpers *)

let build_qcache db qry =
  printf "Building query cache for %d queries and (%d,%d) elem/attrs: This may take a while\n%!" (Array.length qry) (get_db_elem db) (get_db_att db);
  let query_cache = eval_queries_norm true db qry    in
  printf "Query cache built\n%!";
  query_cache

(* Standard generator, loads the database and builds the query cache *)
let mk_exp_data db_gen qry_gen () =
  let (ndb, db_schema, db)    = db_gen                        in
  let db                      = db     ()                     in
  let (nqr, qry)              = qry_gen db_schema             in
  let qcache                  = build_qcache db qry           in

  (* Complemented stuff *)
  let c_qry        = complement_queries qry                   in
  let c_qcache     = complement_qcache  qcache                in
  let att_map      = build_att_map db_schema                  in
  let b_qry        = to_bin_queries att_map c_qry             in
  {
    sd_info    = mk_db_info ndb db att_map;
    sd_queries = b_qry;
    sd_qcache  = c_qcache;
  }

