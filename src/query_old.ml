(* Note we may end up generating the same twice, althought it is not likely *)
let generate_literal n db_schema =
  let att    = Random.int n                      in
  let ai     = db_schema.(att)                   in
  let av     = Random.int (ai.att_max + 1)       in
  if Random.bool ()
  then LEq (att, av)
  else LNeq (att, av)

(* Naive *)
(* let generate_lin_att s a = *)
(*   if s = 1 then *)
(*     PVar a *)
(*   else *)
(*     NVar a *)

(* let generate_lin_query n_att n = *)
(*   let q1 = n mod (n_att * 2)                         in *)
(*   let p1 = q1 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let q2 = n mod (n_att * 2)                         in *)
(*   let p2 = q2 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let q3 = n mod (n_att * 2)                         in *)
(*   let p3 = q3 mod 2                                  in *)
(*   let n  = n / (n_att * 2)                           in *)
(*   let qs = n mod 2                                   in *)
(*   let inner = (generate_lin_att p1 (q1 / 2), generate_lin_att p2 (q2 / 2), generate_lin_att p3 (q3 / 2)) in *)
(*   if qs = 1 then *)
(*     NQuery inner *)
(*   else *)
(*     PQuery inner *)

(* let generate_lin_queries n_atts n_q = *)
(*   Array.init n_q (generate_lin_query n_atts) *)

(* (\* Note that this only generates the positive queries *\) *)
(* let generate_all_queries n_atts = *)
(*   generate_lin_queries n_atts (n_atts * n_atts * n_atts * 2 * 2 * 2) *)

let to_bin_literal att_map lit = match lit with
  | LEq  (a, v) ->
    let (a_pos, ne) = att_map.(a) in
    (* Little hack to support binary attributes *)
    if ne = 0 then
      (if v = 1 then PVar a_pos else NVar a_pos)
    else
      PVar (a_pos + v)

  | LNeq (a, v) ->
    let (a_pos, ne) = att_map.(a) in
    (* Little hack to support binary attributes *)
    if ne = 0 then
      (if v = 1 then NVar a_pos else PVar a_pos)
    else
      NVar (a_pos + v)

let to_bin_query att_map query = match query with
  | PQuery (l1, l2, l3) ->
    let bl1 = to_bin_literal att_map l1 in
    let bl2 = to_bin_literal att_map l2 in
    let bl3 = to_bin_literal att_map l3 in
    BPQuery (bl1, bl2, bl3)

  | NQuery (l1, l2, l3) ->
    let bl1 = to_bin_literal att_map l1 in
    let bl2 = to_bin_literal att_map l2 in
    let bl3 = to_bin_literal att_map l3 in
    BNQuery (bl1, bl2, bl3)

(* Get the first half of an array, that is, without the complements *)
let remove_complements q =
  let l = Array.length q / 2 in
  Array.sub q 0 l

(* Perform the normalization in place *)
let ev_norm_in_place n (dist : float array) : unit =
  let n = float_of_int n in
  Util.map_in_place (fun r -> r /. n) dist

let eval_queries_norm verbose db queries =
  let db_length = Array.length db            in
  let n_queries = Array.length queries       in
  let p_time    = ref (Unix.gettimeofday ()) in

  (* This is the same than eval_queries but with the print

     The performance here is not very good, but indeed in large
     databases we are having cache trouble for sure *)

    (* (Parmap.array_float_parmapi ~ncores:n_cores (fun idx q -> *)
  let res =
    (Parmap.array_parmapi ~ncores:Params.n_cores (fun idx q ->
    (* (Array.mapi (fun idx -> fun q -> *)
    let report = 1000 in
    if verbose && (idx mod report) = 0 then
      let a_time    = Unix.gettimeofday ()                        in
      let secs      = (a_time -. !p_time)                         in
      let q_by_s    = (float_of_int report) /. secs               in
      let h_r       = (float_of_int (n_queries - idx)) /. (q_by_s *. 3600.0)    in
      p_time       := a_time;
      Printf.printf "**** Evaluating query %d at %.2f q/sec. %.2f hours remaning\n%!" idx q_by_s h_r
    else
      ();
    eval_query db q) queries)
  in
  ev_norm_in_place db_length res;
  res

let string_of_lt l = match l with
  | PVar n -> sprintf " %3d" n
  | NVar n -> sprintf "!%3d" n

let string_of_tm (l1, l2, l3) =
  sprintf "%s /\\ %s /\\ %s"
    (string_of_lt l1)
    (string_of_lt l2)
    (string_of_lt l3)

let print_query i q = match q with
  | BPQuery tm -> printf "%3d: + %s\n" i (string_of_tm tm)
  | BNQuery tm -> printf "%3d: - %s\n" i (string_of_tm tm)
