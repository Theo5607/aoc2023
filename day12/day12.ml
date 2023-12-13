let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let set pres x c =
  String.sub pres 0 x ^ (String.make 1 c) ^ String.sub pres (x + 1) (String.length pres - x - 1)

let est_complet pres =
  String.fold_left (fun acc e -> acc && e <> '?') true pres

let trace pres =
  let c = ref 0 in
  let l = ref [] in
  for i = 0 to String.length pres - 1 do
    if pres.[i] <> '#' && !c <> 0 then (l := !l@[!c]; c := 0)
    else if pres.[i] = '#' then incr c
  done;
  if !c <> 0 then l := !l@[!c];
  !l

let rec aux l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _, [] -> true
  | t1::q1, t2::q2 -> if t1 > t2 then false else aux q1 q2

let est_admissible indic pres =
  let t = trace pres in
  List.fold_left max min_int indic > List.fold_left max min_int t && aux t indic

let etend indic pres x c =
  if pres.[x] = '?' then (
    let pres' = set pres x c in
    if est_admissible indic pres' then Some pres' else None
  )
  else None

let resout indic pres =
  let rec explore p x =
    if x = String.length p then Some p
    else if p.[x] <> '?' && est_admissible indic pres then explore p (x + 1)
    else if est_admissible indic pres then match etend indic p x '.', etend indic p x '#' with
    | None, None -> None
    | p', None -> explore (Option.get p') (x + 1)
    | None, p' -> explore (Option.get p') (x + 1)
    | p', p'' -> match explore (Option.get p') (x + 1), explore (Option.get p'') (x + 1) with
                 | None, None -> None
                 | a, None -> a
                 | None, b -> b
                 | a, b -> failwith "erreur"
    else None
  in explore pres 0

(*)let poss pres indic =
  let h = Hashtbl.create 42 in
  let c = ref 0 in
  let rec aux i p =
    print_string p; print_char '\n';
    if trace p = indic && i = String.length p then (incr c; 1)
    else (
      if i < String.length p && p.[i] <> '?' then (
        if Hashtbl.mem h (trace p, i)
        then (incr c; Hashtbl.find h (trace p, i))
        else (let x = aux (i + 1) p in Hashtbl.add h (trace p, i) x; x;))

      else if i < String.length p then 
        (if Hashtbl.mem h (trace (set p i '.'), i)
        then (Hashtbl.find h (trace (set p i '.'), i))
        else (let x = aux (i + 1) (set p i '.') in Hashtbl.add h (trace (set p i '.'), i) x; x)) +
        (if Hashtbl.mem h (trace (set p i '#'), i)
        then (Hashtbl.find h (trace (set p i '#'), i))
        else (let x = aux (i + 1) (set p i '#') in Hashtbl.add h (trace (set p i '#'), i) x; x))
      else 0
    )
  in aux 0 pres*)

let poss pres indic =
  let c = ref 0 in
  let rec aux i p =
    if trace p = indic && i = String.length p then incr c
    else (
      if i < String.length p && p.[i] <> '?' then aux (i + 1) p
      else if i < String.length p then (
        if est_admissible indic (set p i '#') then aux (i + 1) (set p i '#'); 
        if est_admissible indic (set p i '.') then aux (i + 1) (set p i '.'))
      )
  in aux 0 pres; !c

let trans t =
  let t' = Array.make (Array.length t) ("", []) in
  for i = 0 to Array.length t - 1 do
    let s = ref "" in
    for j = 0 to 3 do
      s := !s ^ (t.(i) |> fst) ^ "?"
    done;
    s := !s ^ (t.(i) |> fst);
    let l = snd t.(i) in
    let l' = [l; l; l; l; l] in
    t'.(i) <- (!s, l' |> List.concat)
  done;
  t'
  
let main =
  let file = read_lines (open_in "day12.txt") |> Array.of_list in
  let t = file |> Array.map (fun e -> let t = soc ' ' e in (t.(0), t.(1)))
  |> Array.map (fun (a,b) -> (a, String.split_on_char ',' b |> List.map int_of_string)) |> trans in
  (*)Array.fold_left (fun acc (a, b) -> acc + poss a b) 0 (trans t)*)
  let k = ref 0 in
  for i = 0 to 99 do
    k := !k + poss (t.(i) |> fst) (t.(i) |> snd)
  done;
  !k
