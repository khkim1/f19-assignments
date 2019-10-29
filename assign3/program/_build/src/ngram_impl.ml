open Core

exception Unimplemented

type ngram = string list
type ngram_map = (ngram, string list) Map.Poly.t
type word_distribution = float String.Map.t

let rec remove_last_impl1 (l : string list) : string list =
  match l with
  | [] -> []
  | x :: xs ->
    if xs = [] then []
    else x :: remove_last_impl1 xs
;;

assert (remove_last_impl1 ["a"] = []);
assert (remove_last_impl1 ["a"; "b"] = ["a"]);
assert (remove_last_impl1 [] = []);
;;

let remove_last_impl2 (l : string list) : string list =
  List.filteri l ~f:(fun (i : int) (s : string) : bool ->
    i <> (List.length l) - 1
  )
;;

assert (remove_last_impl2 ["a"] = []);
assert (remove_last_impl2 ["a"; "b"] = ["a"]);
assert (remove_last_impl2 [] = []);
;;

(*List.rev_map ~f:(List.rev)*)
let compute_ngrams (l : string list) (n : int) : string list list =
   List.rev (List.map ~f:(List.rev)
    (List.fold_left l ~init:[[]] ~f:(fun (ngrams : string list list) (s : string) : string list list ->
      match ngrams with
      | [] -> []
      | x :: xs ->
        if (List.length x) < n then [s :: x]
        else (s :: (remove_last_impl2 x)) :: ngrams
      )
    )
  )
;;
assert (compute_ngrams ["a"; "b"; "c"] 2 = [["a"; "b"]; ["b"; "c"]]);
assert (compute_ngrams ["a"; "b"; "c"] 1 = [["a"]; ["b"]; ["c"]]);
;;


let ngram_to_string ng =
  Printf.sprintf "[%s]" (String.concat ~sep:", " ng)
;;

let ngram_map_new () : ngram_map =
  Map.Poly.empty
;;

let ngram_map_add (map : ngram_map) (ngram : ngram) : ngram_map =
  match List.split_n ngram ((List.length ngram) - 1) with
  | (k, v) ->
    (match Map.Poly.find map k with
     | Some cur_v -> Map.Poly.set map ~key:k ~data:(List.append cur_v v)
     | None -> Map.Poly.set map ~key:k ~data:v
    )
;;

let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"; "b"] in
  match Map.Poly.find map ["a"] with
  | Some v -> assert (v = ["b"])
  | None -> assert false;
;;
let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"] in
  match Map.Poly.find map [] with
  | Some v -> assert (v = ["a"])
  | None -> assert false;
;;
let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"; "b"; "c"] in
  let map = ngram_map_add map ["b"; "c"; "d"] in
  let map = ngram_map_add map ["a"; "b"; "d"] in
  (match Map.Poly.find map ["a"] with
  | Some v -> assert false
  | None -> assert true);
  (match Map.Poly.find map ["a"; "b"] with
  | Some v -> assert (v = ["c"; "d"])
  | None -> assert false);
  (match Map.Poly.find map ["b"; "c"] with
  | Some v -> assert (v = ["d"])
  | None -> assert false);
;;

let wd_new () : word_distribution =
  String.Map.empty
;;

let ngram_map_distribution (map : ngram_map) (ngram : ngram)
  : word_distribution option =
  match Map.Poly.find map ngram with
  | None -> None
  | Some v -> Some (List.fold_left v ~init:(wd_new ()) ~f:(fun (wd : word_distribution) (s : string) : word_distribution ->
    match String.Map.find wd s with
    | None -> String.Map.set wd ~key:s ~data:(1. /. (float_of_int (List.length v)))
    | Some p -> String.Map.set wd ~key:s ~data:(p +. (1. /. (float_of_int (List.length v))))
  )
)
;;

let distribution_to_string (dist : word_distribution) : string =
  Sexp.to_string_hum (String.Map.sexp_of_t Float.sexp_of_t dist)
;;

let sample_distribution (dist : word_distribution) : string =
  Random.self_init ();
  let (rnum : float) = Random.float 1. in
    match (String.Map.fold dist ~init:("d", 0.) ~f:(fun ~key ~data (sample, p_sum)->
      if (rnum <= p_sum +. data) && (rnum > p_sum) then
        (key, p_sum +. data)
      else
        (sample, p_sum +. data)
    )) with
    | (sample, p) -> sample
;;

(* let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"; "b"; "c"] in
  let map = ngram_map_add map ["a"; "b"; "c"] in
  let map = ngram_map_add map ["a"; "b"; "d"] in
  let ngram_dist = ngram_map_distribution map ["a"; "b"] in
    (match ngram_dist with
    | None -> assert false
    | Some d -> match (String.Map.find d "c") with
      | None -> assert false
      | Some v -> Printf.printf "%.1f" v);
    (match ngram_dist with
    | None -> assert false
    | Some d -> Printf.printf "%s" (sample_distribution d));
;; *)


let rec sample_n (map : ngram_map) (ng : ngram) (n : int) : string list =
  if n = 0 then
    []
  else
    match (ngram_map_distribution map ng) with
    | None -> []
    | Some dist ->
      (match ng with
       | [] -> let s = (sample_distribution dist) in List.append [s] (sample_n map [] (n - 1))
       | x :: xs -> let s = (sample_distribution dist) in List.append [s] (sample_n map (List.append xs [s]) (n - 1))
      )
;;
