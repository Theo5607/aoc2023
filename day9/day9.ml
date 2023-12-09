let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let calc_hist tab =
  let l = ref [tab] in
  while not (!l |> List.hd |> List.fold_left (fun acc e -> acc && e = 0) true) do 
    let t = !l |> List.hd |> Array.of_list in
    let nl = ref [] in
    for i = 0 to Array.length t - 2 do
      nl := (t.(i + 1) - t.(i))::(!nl)
    done;
    l := (!nl |> List.rev)::(!l) 
  done;
  !l

let calc_val hist =
  hist |> List.tl 
  |> List.fold_left (fun acc e -> (e |> List.hd) - acc) 0

let main =
  let file = read_lines (open_in "day9.txt") |> Array.of_list 
  |> Array.map (fun e -> String.split_on_char ' ' e |> List.map int_of_string) in
  file |> Array.fold_left (fun acc e -> acc + (e |> calc_hist |> calc_val)) 0
