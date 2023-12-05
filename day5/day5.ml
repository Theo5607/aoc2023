let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

(*****************)
let get_seeds file =
  let a = file.(0) |> String.split_on_char ':' |> (function [a; b] -> b | _ -> failwith "read_error") |> soc ' ' in
  Array.init (Array.length a - 1) (fun i -> a.(i + 1)) |> Array.map int_of_string

let matrix_par file =
  let l_all = ref [] in
  let l = ref [] in
  for i = 2 to Array.length file - 1 do
    if (file.(i) |> String.ends_with "map:") then (l_all := (!l |> List.rev |> Array.of_list)::(!l_all) ; l := [];)
    else if (file.(i) <> "" && not (file.(i) |> String.ends_with "map:")) then
        try
        Scanf.sscanf file.(i) "%d %d %d" (fun a b c -> 
          let d = Array.make 3 0 in d.(0) <- a; d.(1) <- b; d.(2) <- c;
          l := d::(!l))
        with Stdlib.Scanf.Scan_failure _ -> ()
  done;
  l_all := (!l |> List.rev |> Array.of_list)::(!l_all);
  !l_all |> List.rev |> Array.of_list

let res file =
  let seeds = get_seeds file in
  let matrix = matrix_par file in
  for i = 0 to Array.length seeds - 1 do
    for j = 0 to Array.length matrix - 1 do
      let flag = ref true in
      for k = 0 to Array.length matrix.(j) - 1 do
        let dest, source, range = matrix.(j).(k).(0), matrix.(j).(k).(1), matrix.(j).(k).(2) in
        if !flag && seeds.(i) >= source && seeds.(i) <= (source + range) then (
          seeds.(i) <- dest + (seeds.(i) - source);
          flag := false;
        )
      done;
    done;
  done;
  seeds |> Array.fold_left min max_int
(*******************)

let get_plages file =
  let seeds = get_seeds file in
  let p = Array.make ((Array.length seeds) / 2) (0, 0) in
  for i = 0 to Array.length p - 1 do
    p.(i) <- (seeds.(2*i), seeds.(2*i + 1))
  done;
  p

let res2 file =
  let p = get_plages file in
  let q = ref (Queue.create ()) in
  p |> Array.iter (fun e -> Queue.add e (!q));
  let matrix = matrix_par file in
  for i = 0 to Array.length matrix - 1 do
    let nq = Queue.create () in
    while not (!q |> Queue.is_empty) do
      let seed, range_seed = Queue.pop !q in
      let flag = ref true in
      for k = 0 to Array.length matrix.(i) - 1 do
        let dest, source, range = matrix.(i).(k).(0), matrix.(i).(k).(1), matrix.(i).(k).(2) in
        if !flag && seed + range_seed - 1 >= source && seed < source then
          (Queue.add (dest, seed + range_seed - source) nq; Queue.add (seed, source - seed) !q; flag := false)
        else if !flag && seed <= source + range - 1 && seed + range_seed - 1 > source + range then
          (Queue.add (dest + seed - source, source + range - seed) nq; Queue.add (source + range, seed + range_seed - source - range) !q; flag := false)
        else if !flag && seed >= source && seed + range_seed - 1 <= source + range - 1 then
          (Queue.add (dest + seed - source, range_seed) nq; flag := false)
        else if !flag && seed < source && seed + range_seed - 1 > source + range - 1 then
          (Queue.add (dest, range) nq; Queue.add (seed, source - seed) !q; Queue.add (source + range, seed + range_seed - source - range) !q; flag := false)
      done;
      if !flag then Queue.add (seed, range_seed) nq 
    done;
    q := Queue.copy nq
  done;
  let l = ref [] in
  while not (!q |> Queue.is_empty) do
    l := (Queue.pop !q)::(!l)
  done;
  !l |> List.map fst |> List.fold_left min max_int

let main =
  let file = read_lines (open_in "day5.txt") |> Array.of_list in
  res2 file
