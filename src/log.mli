(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* We really want to use batteries instead of reimplementing
   this...
*)

type ctx = {
  dir_name : string;
  log_file : out_channel;
}

val new_ctx    : unit -> ctx
val close_ctx  : ctx -> unit

val log        : ctx -> ('a, out_channel, unit) format -> 'a
val new_dir    : ctx -> string -> ctx

val open_file  : ctx -> string -> out_channel
val close_file : out_channel -> unit

(* Top level log *)
val tl         : ctx -> out_channel
