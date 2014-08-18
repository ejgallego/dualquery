(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(* ε₀ utilities *)

(* Calculate epsilon_0 value: eta t s n *)
val epsilon_0        : float -> int -> int -> int -> float

(* compute sample to meet certain epsilon: eps -> eta -> t -> n -> sample *)
val calc_sample_eps0 : float -> float -> int -> int -> int
val calc_iter_eps0   : float -> float -> int -> int -> int

(* (ε,δ) utilities *)

(* Compute (epsilon,delta) value: delta eta t s n *)
val epsilon_delta     : float -> float -> int -> int -> int -> float
val epsilon_delta_b1  : float -> float -> int -> int -> int -> float
val epsilon_delta_b2  : float -> float -> int -> int -> int -> float

(* compute sample to meet certain epsilon: delta -> eps -> eta -> t -> n -> sample *)
val calc_sample_eps_delta    : float -> float -> float -> int -> int -> int
val calc_sample_eps_delta_b1 : float -> float -> float -> int -> int -> int
val calc_sample_eps_delta_b2 : float -> float -> float -> int -> int -> int

(* compute steps to meet certain epsilon: delta -> eps -> eta -> sample -> n -> steps *)
val calc_steps_eps_delta : float -> float -> float -> int -> int -> int

(* compute eta to meet certain epsilon: delta -> eps -> step -> *)
(* sample -> n -> eta *)
val calc_eta_eps_delta : float -> float -> int -> int -> int -> float


