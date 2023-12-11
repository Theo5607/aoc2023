let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let expendre file =
  let l = ref [] in
  for i = 0 to Array.length file - 1 do
    if not (String.contains file.(i) '#') then
      for j = 0 to 1000000 do
        l := (String.make (String.length file.(i)) '.') :: (!l)
      done
    else l := (file.(i))::(!l)
  done;
  let t = !l |> List.rev |> Array.of_list in
  let i = ref 0 in
  while !i < String.length t.(0) do
    if Array.fold_left (fun acc e -> acc && e.[!i] <> '#') true t then (
      Array.iteri (fun j e -> t.(j) <- String.sub e 0 (!i + 1) ^ (String.make 1000000 '.') ^ String.sub e (!i + 1) (String.length t.(j) - !i - 1)) t; i := !i + 1000000);
    incr i
  done;
  t 

let calc_dist file =
  let l_ind = ref [] in
  let c = ref 0 in
  for i = 0 to Array.length file - 1 do
    for j = 0 to String.length file.(0) - 1 do
      if file.(i).[j] = '#' then l_ind := (i, j)::(!l_ind)
    done;
  done;
  let t = !l_ind |> Array.of_list in
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 1 do
      let min1 = min (t.(i) |> fst) (t.(j) |> fst) in
      let max1 = max (t.(i) |> fst) (t.(j) |> fst) in
      for k = min1 to max1 do
        if not (String.contains file.(k) '#') then c := !c + 999999
      done;
      let min2 = min (t.(i) |> snd) (t.(j) |>snd) in
      let max2 = max (t.(i) |> snd) (t.(j) |> snd) in
      for k = min2 to max2 do
        if Array.fold_left (fun acc e -> acc && e.[k] <> '#') true file then c := !c + 999999
      done;
      if i <> j then c := !c + abs ((t.(i) |> fst) - (t.(j) |> fst)) + abs ((t.(i) |> snd) - (t.(j) |> snd))
    done;
  done;
  !c / 2


let main =
  let file = read_lines (open_in "day11.txt") |> Array.of_list in
  calc_dist file 
