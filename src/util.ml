(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

let mapi_in_place (f : int -> float -> float) a =
  (* Array.mapi  f a *)
  Array.iteri (fun idx -> fun v -> Array.unsafe_set a idx (f idx v)) a;
  a

let foldi_left f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f i !r (Array.unsafe_get a i)
  done;
  !r

(* Get the closed integer of a float *)
let best_int_of_float n =
  if (ceil n) -. n > n -. (floor n) then
    int_of_float n
  else
    int_of_float (n +. 1.0)

(* Taken from http://rosettacode.org/wiki/Nth_root#OCaml *)
let nthroot ~n ~a ?(tol=0.001) () =
   let nf = float n in let nf1 = nf -. 1.0 in
   let rec iter x =
      let x' = (nf1 *. x +. a /. (x ** nf1)) /. nf in
      if tol > abs_float (x -. x') then x' else iter x' in
   iter 1.0

let rec take n list = match list with
  | [] -> []
  | x :: xs -> if n = 0 then []
    else x :: (take (n - 1) xs)
