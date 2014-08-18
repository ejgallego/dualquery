(* Copyright (c) 2014, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

(************************************************************************

Definiton of experiments for the ICML 2014 paper.

*)

open Exp

module D = Data
module C = Qcache

(**********************************************************************)
(* A few data sources *)
(* let random_s  = mk_exp_data (D.random 200 2000) (Q.random 2000) *)
(* let rbias500k = mk_exp_data (D.ran_bias 500 500000) (Q.prandom 500000) *)

(* let msadult5k = mk_exp_data  D.msadult (Q.prandom 50000) *)

let net_data     = D.mk_exp_data D.network   (D.ran_queries 500000)
let net_data_s   = D.mk_exp_data D.network_s (D.ran_queries 50000)

let adult_data     = D.mk_exp_data D.adult     (D.ran_queries 300000)
let adult_data_s   = D.mk_exp_data D.adult     (D.ran_queries 30000)
let adult_data_red = D.mk_exp_data D.adult_red (D.ran_queries 5000)

let ttt_data     = D.mk_exp_data D.tictactoe (D.ran_queries 10000)
let ttt_bin_data = D.mk_exp_data D.ttt_bin   (D.ran_queries 10000)

(* small helpers *)
let d_elem data = data.Dq.sd_info.Db.db_elem

let o_zero data = Oracle.mk_zero_oracle   data.Dq.sd_info.Db.db_bin_att
let o_rand data = Oracle.mk_random_oracle data.Dq.sd_info.Db.db_bin_att

open Db
open Dq
module Q = Query

let mk_rbias_data elem atts nqry =
    let db       = generate_bin_db_bias    atts elem       in
    let qry      = Q.generate_bqueries     atts nqry       in
    let qcache   = Q.eval_bqueries_norm true db qry        in
    let c_qry    = Q.complement_bqueries qry               in
    let c_qcache = Q.complement_qcache   qcache            in
    {
      sd_info    = { db_name = "ran_bias"; db_att = atts; db_bin_att = atts; db_elem = elem; };
      sd_queries = c_qry;
      sd_qcache  = c_qcache;
    }

(**********************************************************************)

module IterHelpers = struct

  (* Parameter sweep, for a fixed epsilon0 *)
  let fixed_eps0 eps eta_min eta_max eta_n step_min step_max step_n elem oracle =

    (* Don't use just one step! *)
    let eta_step  = (eta_max  -. eta_min) /. (float_of_int (eta_n - 1))  in
    let step_step = (step_max - step_min) /  (step_n - 1)                in

    let param_gen = (fun idx ->
      let eta_iter  = idx / step_n                                          in
      let step_iter = idx mod step_n                                        in
      let eta       = eta_min  +. eta_step  *. (float_of_int eta_iter)      in
      let step      = step_min +  step_step *  step_iter                    in
      let sample    = Epsilon.calc_sample_eps0 eps eta step elem            in
      mk_exp_params eta step sample oracle
    )
    in (eta_n * step_n, param_gen)

  let fixed_eps_delta delta eps eta_min eta_max eta_n step_min step_max step_n elem oracle =

    (* Don't use just one step! *)
    let eta_step  = (eta_max  -. eta_min) /. (float_of_int (eta_n - 1))     in
    let step_step = (step_max - step_min) /  (step_n - 1)                   in

    let param_gen = (fun idx ->
      let eta_iter  = idx / step_n                                          in
      let step_iter = idx mod step_n                                        in
      let eta       = eta_min  +. eta_step  *. (float_of_int eta_iter)      in
      let step      = step_min +  step_step *  step_iter                    in
      let sample    = Epsilon.calc_sample_eps_delta delta eps eta step elem in
      mk_exp_params eta step sample oracle
    )
    in (eta_n * step_n, param_gen)


  let epsilon delta eps_min eps_max n_steps elem oracle =
    let eps_step = (eps_max -. eps_min) /. (float_of_int (n_steps - 1)) in

    let param_gen = (fun idx ->
      let eps    = eps_min +. (eps_step *. (float_of_int idx)) in
      let eta    =  0.1 in
      let step   =  200 in
      let sample = Epsilon.calc_sample_eps_delta delta eps eta step elem in
      mk_exp_params eta step sample oracle
    ) in
    param_gen

  (* This time we vary the number of steps *)
  let epsilon2 delta eps_min eps_max n_steps elem oracle =
    let eps_step = (eps_max -. eps_min) /. (float_of_int (n_steps - 1)) in

    let param_gen = (fun idx ->
      let eps    = eps_min +. (eps_step *. (float_of_int idx)) in
      let eta    =  0.1 in
      let sample   =  200 in
      let step = Epsilon.calc_steps_eps_delta delta eps eta sample elem in
      mk_exp_params eta step sample oracle
    ) in
    param_gen

  (* This time we vary the value of eta  *)
  let epsilon3 delta eps_min eps_max n_steps elem oracle =
    let eps_step =  (eps_max -. eps_min) /. (float_of_int (n_steps - 1)) in

    let param_gen = (fun idx ->
      let eps     = eps_min +. (eps_step *. (float_of_int idx)) in
      let step    = 200 in
      let sample  = 200 in
      let eta = Epsilon.calc_eta_eps_delta delta eps step sample elem in
      mk_exp_params eta step sample oracle
    ) in
    param_gen

end

module IH = IterHelpers

(**********************************************************************)
(* Cache writers *)
module Writers = struct

  let net_data        = D.mk_exp_data D.network         (D.ran_queries 2000000)
  let adult_data      = D.mk_exp_data D.adult           (D.ran_queries 2000000)
  let netflix_data    = NfArray.mk_netflix_exp_data "data/nf_data.txt" 2000000

  let netflix_data_500k    = NfArray.mk_netflix_exp_data "data/nf_data.txt"  500000

  let network () =
    let data = net_data () in
    C.save_exp_data data "qcache/net-q2M.qcache"

  let network_queries () =
    let data = C.load_exp_data "qcache/net-q1M.qcache" in
    C.save_exp_queries data "qcache/net-only-queries"

  let adult () =
    let data = adult_data () in
    C.save_exp_data data "qcache/adult-q2M.qcache"

  let netflix_500k () =
    let data = netflix_data_500k () in
    C.save_exp_data data "qcache/netflix-q500K.qcache"

  let netflix () =
    let data = netflix_data () in
    C.save_exp_data data "qcache/netflix-q2M.qcache"

  let all () =
    network ();
    adult   ();
    netflix ()

  let test_bs () =
    let data   = C.load_exp_data ~cutoff:10000 "qcache/net-q1M.qcache" in
    C.save_exp_data data "qcache/net-try.qcache"

end

(**********************************************************************)
(* Simple experiments *)
module Simple = struct

  (* Average over 5 runs *)
  let nr = 5

  let build_simple eta t s f_data =
    let data        = f_data ()                    in
    let oracle      = o_rand data                  in
    let params      = mk_exp_params eta t s oracle in
    do_exp_single nr (data, params)

  let net_s () =
    build_simple 1.5 20 2000 net_data_s

  let adult () =
    build_simple 0.8 20 100 adult_data

  let adult_s () =
    build_simple 0.8 40 1000 adult_data_s

  let adult_red () =
    build_simple 0.4 30 100 adult_data_red

  let ttt () =
    build_simple 1.2 100 300 ttt_data

  let bin () =
    build_simple 0.01 200 80 ttt_bin_data

  let bin_toy () =
    build_simple 0.1 1 0 ttt_bin_data

  let netflix () =
    (* let data   = Netflix.mk_netflix_exp_data "data/nf_data-small.txt" 100000  in *)
    (* let data   = NfArray.mk_netflix_exp_data "data/nf_data.txt" 1000000  in *)

    let data   = C.load_exp_data "qcache/netflix-q2M.qcache" in
    let oracle = o_rand data                                in

    (* Reasonable epsilon of 0.9 *)
    let params = mk_exp_params 0.2 100 278320 oracle           in
    do_exp_single 3 (data, params)

  let net_several () =
    let data   = net_data_s () in
    let oracle = o_rand data   in
    let p_array = [| mk_exp_params 0.01 10 100 oracle;
		     mk_exp_params 0.01 10 280 oracle;
		     mk_exp_params 0.01 10 390 oracle;
		  |] in
    Array.iter (fun p ->
      do_exp_single nr (data, p)
    ) p_array

  let ran_bias () =
    let elem     = 150000                              in
    let data     = mk_rbias_data elem 1000 100000      in
    let oracle   = o_rand data                         in
    let params   = mk_exp_params 1.0 97 1200 oracle    in
    do_exp_single 3 (data, params)

  let ran_bias_mw () =
    let elem     = 10000                               in
    let data     = mk_rbias_data elem 16 1000          in
    let oracle   = o_rand data                         in
    let params   = mk_exp_params 0.4 30 29 oracle      in
    do_exp_single 3 (data, params)

end

(**********************************************************************)
(* Experiments varying the parameters *)
module Param = struct

  let adult_s () =
    let data   = adult_data_s ()                                             in
    let oracle = o_rand data                                                 in
    let elem   = d_elem data                                                 in
    let (s, f) = IH.fixed_eps_delta 0.001 1.0 0.2 2.0 4 20 200 5 elem oracle in
    do_exp_iter s (fun idx -> (3, data, f idx))

  let adult () =
    let data   = C.load_exp_data "qcache/adult-q2M.qcache" in
    let oracle = o_rand data                               in
    let elem   = d_elem data                               in

    let e_array = Array.map (fun eps -> IH.fixed_eps_delta 0.001 eps 0.05 1.0 11 50 200 20 elem oracle)
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |] in

    Array.iter (fun (s, f) ->
      do_exp_iter s (fun idx -> (5, data, f idx))) e_array

  let adult_red () =
    let data   = adult_data_red ()                         in
    let oracle = o_rand data                               in
    let elem   = d_elem data                               in


    (* Simple generator for a fixed epsilon *)
    let exp_gen idx =
      let eps    = float_of_int (idx + 1) in
      let eta    = 0.4                                        in
      let sample = 35 + idx * 5                               in
      let step   = Epsilon.calc_iter_eps0 eps eta sample elem in
      mk_exp_params eta step sample oracle
    in

    do_exp_iter 5 (fun idx -> (3, data, exp_gen idx))

  let network () =
    let data   = C.load_exp_data "qcache/net-q2M.qcache"   in
    let oracle = o_rand data                               in
    let elem   = d_elem data                               in

    let e_array = Array.map (fun eps -> IH.fixed_eps_delta 0.001 eps 0.05 1.0 11 200 1000 20 elem oracle)
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |] in

    Array.iter (fun (s, f) ->
      do_exp_iter s (fun idx -> (5, data, f idx))) e_array

  let net_s ()    =
    let data   = net_data_s () in
    let oracle = o_rand data   in
    let elem   = 494021        in
    let (s, f) = IH.fixed_eps_delta 0.001 1.0 1.7 2.2 3 70 210 3 elem oracle in
    do_exp_iter s (fun idx -> (3, data, f idx))

  let net_s2 ()   =
    let data   = net_data_s () in
    let oracle = o_rand data   in
    let elem   = 494021        in
    let (s, f) = IH.fixed_eps_delta 0.001 1.0 0.15 0.45 3 500 2000 4 elem oracle in
    do_exp_iter s (fun idx -> (3, data, f idx))

  let nf_s ()     =
    let data   = NfArray.mk_netflix_exp_data "data/nf_data-small.txt" 500000 () in
    let oracle = o_rand data                                                    in
    let elem   = 480489                                                         in
    let (s,f)  = IH.fixed_eps_delta 0.001 1.0 1.8 2.2 3 70 210 3 elem oracle    in
    do_exp_iter s (fun idx -> (3, data, f idx))

  let netflix () =
    (* let data   = C.load_exp_data "qcache/netflix-q2M.qcache"             in *)
    let data   = C.load_exp_data ~cutoff:100000 "qcache/netflix-q500K.qcache"    in
    let oracle = o_rand data                                                    in
    let elem   = d_elem data                                                    in

(*
    let param  = [| (0.1, 150); (0.1, 200); (0.1, 250); (0.1, 300);
		    (0.2, 100); (0.1, 150); (0.1, 200);
		    (0.3, 75);  (0.3, 100); (0.3, 150) |] in
*)
    (* Ahh, messed up the previous one *)
    let param  = [| (0.2, 150); (0.2, 200) |] in

    let n = Array.length param in

    (* Several runs for different eta*)
    do_exp_iter n (fun idx ->
      let eta  = fst (param.(idx)) in
      let step = snd (param.(idx)) in
      let sample = Epsilon.calc_sample_eps_delta 0.001 1.0 eta step elem in
      let param  = mk_exp_params eta step sample oracle in
      (2, data, param))

  let netflix_z () =
    (* More or less quick *)
    let data   = C.load_exp_data ~cutoff:100000 "qcache/netflix-q500K.qcache"    in
    let oracle = o_zero data                                                    in
    let elem   = d_elem data                                                    in

    (* let (s, f) = IH.fixed_eps_delta 0.001 1.0 0.4 2.0 9 100 200 5 elem oracle   in *)
    let (s, f) = IH.fixed_eps_delta 0.001 1.0 1.0 2.0 6 200 400 5 elem oracle   in
    do_exp_iter s (fun idx -> (2, data, f idx))

  let ran_bias () =
    let elem     = 100000                                                       in
    let data     = mk_rbias_data elem 8000 100000                               in
    let oracle   = o_rand data                                                  in
    let (s, f)   = IH.fixed_eps_delta 0.001 1.0 0.2 2.0 5 20 200 5 elem oracle  in
    do_exp_iter s (fun idx -> (2, data, f idx))

end

(**********************************************************************)
(* Epsilon vs accuracy vs rutime *)
module Epsilons = struct

  (* TODO, we want a method for fixing the parameters from a given
     eps *)
  let adult_s () =
    let data   = adult_data_s ()                        in
    let oracle = o_rand data                            in
    let elem   = d_elem data                            in
    let f      = IH.epsilon 0.001 0.2 1.0 5 elem oracle in
    do_exp_iter 5 (fun idx -> (2, data, f idx))

  let net_s ()  =
    let data   = NfArray.mk_netflix_exp_data "data/nf_data-small.txt" 500000 () in
    let oracle = o_rand data                                                    in
    let elem   = 480489                                                         in
    let f      = IH.epsilon 0.001 1.0 5.0 5 elem oracle                         in
    do_exp_iter 3 (fun idx -> (2, data, f idx))


  let net_s_fast () =
    let data  = C.load_exp_data "qcache/net-q2M.qcache"    in
    let oracle = o_rand data                               in
    let elem  = d_elem data                                in
    let f     = IH.epsilon2 0.001 0.25 1.0 4 elem oracle   in
    do_exp_iter 4 (fun idx -> (4, data, f idx))
end

(**********************************************************************)
(* Number of queries vs accuracy *)
module Queries  = struct

  let network () =
    let qrys = Array.init 50 (fun i -> (i + 1) * 40000) in

    do_exp_iter (Array.length qrys) (fun idx ->
      let nqry = qrys.(idx)                                             in
      let data = C.load_exp_data ~cutoff:nqry "qcache/net-q2M.qcache"   in
      let oracle = o_rand data                                          in
      let params = mk_exp_params 1.2 170 1750 oracle                    in
      (2, data, params)
    )

  let netflix () =

    let qrys   = Array.init 50 (fun i -> (i + 1) * 40000)               in
    (* let params = mk_exp_params 1.2 120 4500                             in *)

    do_exp_iter (Array.length qrys) (fun idx ->
      let nqry   = qrys.(idx)                                                 in
      let data   = C.load_exp_data ~cutoff:nqry "qcache/netflix-q2M.qcache"   in
      let oracle = o_rand data                                                in
      let params = mk_exp_params 1.2 120 4500 oracle                          in
      (3, data, params)
    )

  let netflix_z () =

    (* Exponential query grow *)
    let qrys   = Array.init 19 (fun i ->
      let n  = float_of_int (i + 1) in
      let nq = 2.0 ** n             in
      min 500000 (int_of_float nq)
    ) in

    do_exp_iter (Array.length qrys) (fun idx ->
      let nqry   = qrys.(idx)                                                 in
      let data   = C.load_exp_data ~cutoff:nqry "qcache/netflix-q500K.qcache" in
      let oracle = o_zero data                                                in
      let params = mk_exp_params 1.2 200 958 oracle                           in
      (3, data, params)
    )

end

(**********************************************************************)
(* No. attributes vs accuracy and runtime *)
module Attributes = struct

  (* Returns true if the query is using all attributes less than n *)
  let q_less q n =
    let q_a l = match l with
      | Q.PVar x -> x < n
      | Q.NVar x -> x < n
    in
    match q with
      | Q.BPQuery (l1, l2, l3) -> q_a l1 && q_a l2 && q_a l3
      | Q.BNQuery (l1, l2, l3) -> false

  let netflix () =

    let qlist q k = Array.fold_left (fun (l, n) q ->
      if q_less q k then
	(n :: l, (n+1))
      else
	(l,      (n+1))) ([], 0) q in

    let data      = C.load_exp_data "qcache/netflix-q500K.qcache"              in

    let n_steps   = 12                                                         in
    (* 6000 + 11 * 1000 < 17770 *)
    let s_fun i   = 6000 + i * 1000                                            in

    (* We take the first 20000 queries to have k atts *)
    let restrict  = 20000 in

    (* Array with the list of queries to experiment with *)
    let n_queries = Array.init n_steps (fun k ->
      let atts = s_fun k  in
      Util.take restrict (fst (qlist data.sd_queries atts))
    ) in

    (* Array.iteri (fun k v -> Printf.printf "%d: %d\n" (s_fun k) (List.length v)) n_queries *)

    (* Build the actual queries and cache arrays *)
    let qry       = Array.map (fun kl ->
      Array.of_list (List.map (fun i -> data.sd_queries.(i) ) kl)) n_queries in

    let qcache    = Array.map (fun kl ->
      Array.of_list (List.map (fun i -> data.sd_qcache.(i) )  kl)) n_queries in

    let c_qry     = Array.map Q.complement_bqueries qry                      in
    let c_qcache  = Array.map Q.complement_qcache   qcache                   in

    do_exp_iter n_steps (fun idx ->
      (*  *)
      let data = {
	sd_info    = { data.sd_info with
	  db_att     = s_fun idx;
	  db_bin_att = s_fun idx;
	};
	sd_queries = c_qry.(idx);
	sd_qcache  = c_qcache.(idx);
      } in

      (* S depends on the number of attributes. *)
      let eta    = 0.4                                                             in
      let sample = 3 * s_fun idx                                                   in
      let times  = 3                                                               in
      let step   = Epsilon.calc_steps_eps_delta 0.001 1.0 0.4 sample (d_elem data) in
      let oracle = o_rand data                                                     in
      let param  = mk_exp_params eta step sample oracle                            in
      (times, data, param)
    )

  (* We experiment with random biased data varying the number of
     attributes *)
  let ran_bias () =
    let atts = [| 50; 250; 500; 1000; 2000; 4000; 8000; 16000; 32000|] in
    let nexp = Array.length atts                                       in
    let elem = 100000                                                  in
    let nqry = 100000                                                  in

    do_exp_iter nexp (fun idx ->
      let atts   = atts.(idx)                        in
      let data   = mk_rbias_data elem atts nqry      in
      let oracle = o_rand data                       in
      let param  = mk_exp_params 1.0 100 500 oracle  in
      (2, data, param)
    )

  let ran_bias_ba () =
    Params.timeout := 600;

    let atts   = [ ( 500,  [  300;  400;  500;  600;  800; 1000; 1200 ]);
		    (1000, [  800;  900; 1000; 1200; 1300; 1500; 1800; 2000 ]);
		    (2000, [ 1000; 1200; 1500; 1800; 2000; 2250; 2500; 2800; 3000; 3500; 4000 ]);
		 ] in
    let matts  = Array.of_list (List.concat (List.map (fun (x, a) -> List.map (fun e -> (x, e)) a) atts)) in
    (* let atts   = [|  50; 250;  500; 1000; 2000; 4000; 8000; 16000; 32000; 64000; 128000|] in *)
    (* let sample = [| 200; 400; 1000; 2000; 3000; 4000; 5500;  7000;  9000; 14000;  25000|] in *)
    (* let atts   = [| 512000 |] in *)
    (* let sample = [|  70000 |] in *)
    let nexp = Array.length matts                                                     in
    let elem = 100000                                                                 in
    let nqry = 100000                                                                 in

    do_exp_iter nexp (fun idx ->
      let atts   = fst matts.(idx)                              in
      (* let data   = Bineval.mk_ba_rbias_data_slow elem atts nqry in *)
      let data   = Bineval.mk_ba_rbias_data elem atts nqry      in
      let oracle = o_rand data                                  in
      let eta    = 0.3                                          in
      let sample = snd matts.(idx)                              in
      let step   = Epsilon.calc_steps_eps_delta 0.001 1.0 eta sample (d_elem data) in
      let param  = mk_exp_params eta step sample oracle         in
      (3, data, param)
    )

end

(**********************************************************************)
(* No. elements in the db vs accuracy and runtime *)
module Size = struct

  (* Reasonable numbers for query eval *)
  let ran_bias () =
    let nqry = 100000                    in
    let atts = 1000                      in

    do_exp_iter 20 (fun idx ->
      let elem   = (idx + 1) * 10000            in
      let eta    = 1.0                          in
      let step   = 100 + (elem / 2000)          in
      let data   = mk_rbias_data elem atts nqry in
      let sample = Epsilon.calc_sample_eps_delta 0.001 1.0 eta step elem  in
      let oracle = o_rand data                          in
      let param  = mk_exp_params eta step sample oracle in
      (2, data, param)
    )

  let ran_bias_step () =
    let nqry = 100000                    in
    let atts = 1000                      in

    do_exp_iter 20 (fun idx ->
      let elem   = (idx + 1) * 10000            in
      let eta    = 1.0                          in
      let sample = 1200                         in
      let data   = mk_rbias_data elem atts nqry in
      let step   = Epsilon.calc_steps_eps_delta 0.001 1.0 eta sample elem  in
      let oracle = o_rand data                          in
      let param  = mk_exp_params eta step sample oracle in
      (3, data, param)
    )

  (*   do_exp_iter nexp (fun idx -> *)
  (*     let atts = atts.(idx)                        in *)
  (*     let data = mk_rbias_data elem atts nqry      in *)
  (*     (2, data, param) *)
  (*   ) *)

end

(**********************************************************************)
(* Experiments for private ML                                         *)

module ML = struct

  let nf_try () =
    Params.output  := true;
    Params.timeout := 100;

    (* let data   = NfArray.mk_netflix_exp_data "data/nf_data.txt" 1000000  in *)

    let data   = C.load_exp_data "qcache/netflix-q2M.qcache"   in
    let oracle = o_zero data                                   in

    let params = mk_exp_params 1.0 250 706 oracle              in
    do_exp_single 3 (data, params)

end
