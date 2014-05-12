(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Unix
open Printf

type ctx = {
  dir_name : string;
  log_file : out_channel;
}

(* FIXME: Times are GMT... *)
let new_ctx () =
  let t  = gmtime (time ())                                   in
  let ts = sprintf "%04d-%02d-%02d-%02d-%02d-%02d"
                        (t.tm_year + 1900) (t.tm_mon + 1)
                        t.tm_mday t.tm_hour t.tm_min t.tm_sec in
  let ds = "rundq-" ^ ts                                      in
  mkdir ds 0o755;
  {
    dir_name = ds;
    log_file = open_out (ds ^ "/" ^ "log.txt");
  }

let close_ctx ctx =
  close_out ctx.log_file

let log ctx =
  let out = ctx.log_file in
  fprintf out

let new_dir ctx dir =
  let n_dir = ctx.dir_name ^ "/" ^ dir in
  mkdir n_dir 0o755;
  { ctx with dir_name = n_dir; }

let open_file ctx file =
  open_out_gen [Open_append;Open_creat;Open_text] 0o644
    (ctx.dir_name ^ "/" ^ file)

let close_file oc =
  close_out oc

let tl ctx =
  ctx.log_file
