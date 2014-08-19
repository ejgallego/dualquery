(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Db
open Dbdist
open Query

(* Data for a simulation *)
type exp_data = {
  sd_info     : db_info;
  sd_queries  : bin_query array;
  sd_qcache   : float array;
}

(* Parameters for an actual experiment *)
type exp_param = {
  exp_eps     : float;
  exp_steps   : int;
}

type exp_res = db_dist
(* Result for an actual experiment *)
(*
type exp_result = {
  res_db       : db_dist;
  res_timeout  : int;
  res_seconds  : int;
  res_qd_stats : (int * float * float);
}
*)

open Expm

(* val mwem : exp_data -> exp_param -> exp_result *)
let mwem data param init =
  let open Printf in

  let t       = param.exp_steps                    in
  let tf      = float_of_int t                     in

  let elem    = data.sd_info.db_elem               in
  let n       = float_of_int elem                  in

  let tot_eps = param.exp_eps                      in
  let em_eps  = tot_eps /. (2.0 *. tf)             in
  let lap_eps = 2.0 *. tf /. tot_eps               in

  let realdb  = data.sd_qcache                     in
  let qry     = data.sd_queries                    in
  let nqry    = Array.length qry                   in

  printf "Starting MWEM: Eps=%f, T=%d, n=%d, em_eps=%f, lap_eps=%f, Q=%d"
    tot_eps t elem em_eps lap_eps nqry;

  (* Universe size *)
  (* let usize   = Util.pow 2 data.sd_info.db_bin_att in *)

  (* Initial dist *)
  let d       = init                               in

  (* Unused accumulator *)
  (* let avg     = uniform_dist usize                 in *)

  (* We follow HLM and will cache the worst performing qry *)
  let open Hashtbl in
  let qval : (int, float) t = create t in

  for i = 1 to t do
    printf "\nStep: %d\n%!" i;

    (* Multiplying by n *)
    let score idx =
      (* let res =  *)
      abs_float (realdb.(idx) -. eval_bquery d qry.(idx)) *. n (* in *)
      (* printf "Score for query %d: %f \n%!" idx res; *)
      (* res in *)
    in

(*
    let dump_qerror () =
    (* Print error *)
      printf "Query Error: \n";
      Array.iteri (printf "q%2d: %f\n%!") (Array.init nqry score) in
*)

    let rec get_badquery niter =
      let badquery = exp_mech em_eps nqry score         in
      if mem qval badquery then
        if niter = 0 then begin
          printf "[warning] Repeating query selection %d!!!\n" badquery;
          (* dump_qerror (); *)
          badquery, find qval badquery
        end
        else
          get_badquery (niter - 1)
      else
        (* We noise the real value *)
        let m = realdb.(badquery) +. Laplace.lap_noise lap_eps /. n in
        add qval badquery m;
        badquery, m
    in
    let badquery, m = get_badquery 2                    in

    (* qi is [0..1] *)
    let qi        = eval_bquery d qry.(badquery)        in
    printf "Worst query: %d with error %f\n%!" badquery (realdb.(badquery) -. qi);

    let c_err = m -. qi                                          in
    printf "Corrected private error: %f, the update factor will be: %f \n%!" c_err (exp (c_err /. 2.0));

    (* MW update rule *)
    let _mw_update idx v =
      let up_factor =
          exp ( (ev_bquery idx qry.(badquery) ) *.
                c_err /. 2.0 )
      in
      (* printf "Update for %d (%f) with uf: %f\n" idx d.(idx) up_factor; *)
      v *. up_factor
    in

    (* Arbitrary! *)
    let k_l = 20 in
    for k = 1 to k_l do
      let update q m =
        let qi    = eval_bquery d qry.(q) in
        let c_err = (m -. qi) /. 2.0      in
        let mw_update idx v =
          let up_factor = exp ( ev_bquery idx qry.(q) *. c_err)
          in
          v *. up_factor
        in
        Util.mapi_in_place mw_update d;
        d_norm_in_place d
      in
      iter update qval
      (* Util.mapi_in_place _mw_update d; *)
      (* d_norm_in_place d *)
    done;

    (* Add the sum of this one to the result *)
    (* Util.mapi_in_place (fun idx v -> v +. d.(idx)) avg; *)

  done;
  (* For now we just release d *)
  d

  (* Do the average *)
  (* Util.map_in_place (fun v -> v /. tf) avg; *)
  (* avg *)

(***********************************************************************)
(* Non-private version *)

let mw data param init =
  let open Printf in

  let t       = param.exp_steps                    in

  let n       = float_of_int data.sd_info.db_elem  in

  let realdb  = data.sd_qcache                     in
  let qry     = data.sd_queries                    in
  let nqry    = Array.length qry                   in

  (* Initial dist *)
  let d       = init                               in

  for i = 1 to t do
    printf "\nStep: %d\n%!" i;

    (* Multiplying by n *)
    let error    = Array.init nqry (fun idx ->
      abs_float (realdb.(idx) -. eval_bquery d qry.(idx)) *.n) in

    let badquery = Util.foldi_left (fun idx r _ -> if error.(idx) >= error.(r) then idx else r) 0 error  in

    let m  = realdb.(badquery)            in
    let qi = eval_bquery d qry.(badquery) in

    printf "Worst query is %d with error %f\n%!" badquery (realdb.(badquery) -. qi);
    let c_err = m -. qi                                 in

    (* MW update rule *)
    let mw_update idx v =
      let up_factor =
          exp ( (ev_bquery idx qry.(badquery) ) *.
                c_err /. 2.0 )
      in
      (* printf "Update for %d (%f) with uf: %f\n" idx d.(idx) up_factor; *)
      v *. up_factor
    in

    (* Arbitrary! *)
    let k_l = 20 in
    for k = 1 to k_l do
      Util.mapi_in_place mw_update d;
      d_norm_in_place d
    done;

  done;
  d
