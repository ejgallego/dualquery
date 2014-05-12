(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Dq

(** [save_exp_data data f] saves experiment data [data] to the file
    [f] *)
val save_exp_data : exp_data -> string -> unit

val save_exp_queries : exp_data -> string -> unit

(** [load_exp_data f qry_cutoff] loads [qry_cutoff] queries and its
    result from query cache file [f], returning and [exp_data] ready
    to be used by dq. If [qry_cutoff] is -1, it loads all the data in
    the file *)
val load_exp_data : ?cutoff:int -> string -> exp_data
