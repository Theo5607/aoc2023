let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let add_if_not_in h e i =
  if not (Hashtbl.mem h e) then Hashtbl.add h e i

let get_col t i =
  let s = ref "" in
  Array.iter (fun e -> s := !s ^ (String.make 1 e.[i])) t;
  !s

let trans file =
  let l1 = ref [] in
  let l = ref [] in
  let i = ref 0 in
  while !i < Array.length file do
    if file.(!i) |> String.equal "" then (
      l1 := (!l |> Array.of_list)::(!l1);
      l := []
    ) else ( l := (!l)@[file.(!i)] );
    incr i
  done;
  l1 := (!l |> Array.of_list)::(!l1);
  !l1 |> List.rev |> Array.of_list

let detect_sym t =
  let h_l = Hashtbl.create 42 in
  let h_c = Hashtbl.create 42 in
  Array.iteri (fun i e -> add_if_not_in h_l e i) t;
  String.iteri (fun i e -> add_if_not_in h_c (get_col t i) i) t.(0);
  let k = ref (-1) in
  let rg = ref 0 in
  let dec = ref 0 in
  let rg_gen = ref 0 in
  for i = 0 to String.length t.(0) - 2 do
    if Hashtbl.find h_c (get_col t i) = Hashtbl.find h_c (get_col t (i + 1)) then (k := i; rg := i);
    while !k >= 0 && !k + 1 + !dec < String.length t.(0) && Hashtbl.find h_c (get_col t !k) = Hashtbl.find h_c (get_col t (!k + 1 + !dec)) do
      decr k;
      dec := !dec + 2;
    done;
    if !k = 0 || !k + 1 + !dec = String.length t.(0) then (rg_gen := !rg);
    rg := 0; dec := 0; k := -1;
  done;
  if !rg_gen <> 0 then (Printf.printf "colonne %d\n" !rg_gen; !rg_gen + 1) else ( 
    for i = 0 to Array.length t - 2 do
      if Hashtbl.find h_l t.(i) = Hashtbl.find h_l t.(i + 1) then(rg := i; k := i);
      while !k >= 0 && !k + 1 + !dec < Array.length t && Hashtbl.find h_l t.(i) = Hashtbl.find h_l t.(i) do
        decr k;
        dec := !dec + 2;
      done;
      if !k = 0 || !k + 1 + !dec = String.length t.(0) then (rg_gen := !rg);
      rg := 0; dec := 0; k := -1; 
    done;
    Printf.printf "ligne %d\n" !rg_gen; 
    100 * (!rg_gen + 1))

let main =
  let file = read_lines (open_in "day13.txt") |> Array.of_list in
  Array.fold_left (fun acc e -> acc + (detect_sym e)) 0 (trans file)
