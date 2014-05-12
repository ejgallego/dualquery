(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

module Ba = Bigarray
module A1 = Bigarray.Array1
module A2 = Bigarray.Array2
module Ni = Nativeint

(* Space-Efficient two-dimensional large binary arrays *)
type ba_row = (nativeint, Ba.nativeint_elt, Ba.c_layout) A1.t
type ba     = (nativeint, Ba.nativeint_elt, Ba.c_layout) A2.t

let word_size = Sys.word_size

let make d1 d2 default =

  (* Number of words needed to represent all binary elements *)
  let n_words = (d2 / word_size) + 1 in

  Printf.printf "Create array %dx%d=%d words\n%!" d1 n_words (d1 * n_words);

  let array = A2.create Ba.nativeint Ba.c_layout d1 n_words in
  A2.fill array default;
  array

let dim1 = A2.dim1

let get_row = A2.slice_left

let set_bit word n =
  let pos = n mod word_size in
  Ni.logor word (Ni.shift_left Ni.one pos)

let get_bit word n =
  let pos = n mod word_size in
  let bit = Ni.logand Ni.one (Ni.shift_right word pos) in
  if Ni.one = bit then true else false

let get_word db row col =
  let w_pos  = col / word_size       in
  A2.unsafe_get db row w_pos

let put_word db row col w_val =
  let w_pos  = col / word_size       in
  A2.unsafe_set db row w_pos w_val

(* Set and get attributes *)
let att_set db row col =
  let w_val = get_word db row col    in
  let n_val = set_bit w_val col      in
  put_word db row col n_val

let att_get db row col =
  let w_val = get_word db row col    in
  get_bit w_val col

