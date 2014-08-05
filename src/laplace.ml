(* Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

let lap_noise sigma =
  let unif = (Random.float 1.0) -. 0.5 in
  let s = if unif < 0.0 then -1.0 else 1.0 in
    s *. sigma *. log(1.0 -. 2.0 *. (abs_float unif))
