(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* Misc array utils *)
let sum (x : float array) =
  Array.fold_left (+.) 0.0 x

let avg (x : float array) =
  sum x /. float_of_int (Array.length x)

let max (x : float array) =
  Array.fold_left max min_float x

let min (x : float array) =
  Array.fold_left min max_float x

(* Map in place functions *)
let map_in_place (f : float -> float) (a : float array) =
  Array.iteri (fun idx -> fun v -> Array.unsafe_set a idx (f v)) a

let mapi_in_place (f : int -> float -> float) a =
  Array.iteri (fun idx -> fun v -> Array.unsafe_set a idx (f idx v)) a


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

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
