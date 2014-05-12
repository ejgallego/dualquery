(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db

(* We should use modules for this, but anyways... *)

type int_literal    = LEq of int * int | LNeq of int * int

type int_3_marginal = int_literal * int_literal * int_literal

type query          = PQuery of int_3_marginal | NQuery of int_3_marginal

type bin_literal = PVar of int | NVar of int

type bin_three_marginal = bin_literal * bin_literal * bin_literal

type bin_query = BPQuery of bin_three_marginal | BNQuery of bin_three_marginal

val to_bin_queries : att_map -> query array -> bin_query array

val generate_bquery     : int ->        bin_query
val generate_bqueries   : int -> int -> bin_query array

val generate_query      : int -> db_schema  -> query
val generate_queries    : db_schema  -> query

(* Only positive queries *)
val generate_queries   : db_schema -> int -> query array

(* For later *)
(* val generate_lin_queries : int -> int -> query array *)
(* val generate_all_queries : int -> query array *)

val complement_query    : query -> query
val complement_bquery   : bin_query -> bin_query

val complement_queries  : query array -> query array
val complement_bqueries : bin_query array -> bin_query array

val complement_qcache   : float array -> float array

val remove_complements  : 'a array -> 'a array

val eval_bin_elem       : bin_query -> bin_db_row -> float

val eval_elem           : query -> db_row       -> float
val eval_query          : db    -> query        -> float

(* val eval_queries      : db    -> query array  -> float array *)

val eval_queries_norm  : bool -> db -> query array -> float array
val eval_bqueries_norm : bool -> bin_db -> bin_query array -> float array

val print_bqueries : bin_query array -> unit
