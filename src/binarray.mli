(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Space-Efficient two-dimensional large binary arrays *)
type ba_row = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t
type ba     = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array2.t

val make    : int -> int -> nativeint -> ba

val dim1    : ba -> int

val get_row : ba -> int -> ba_row

val att_set : ba -> int -> int -> unit

val att_get : ba -> int -> int -> bool


