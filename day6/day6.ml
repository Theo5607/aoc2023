let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let res =
  let time = [|55826490|] in
  let dist = [|246144110121111|] in
  let c = ref 0 in
  let p = ref 1 in
  for i = 0 to 0 do
    for j = 1 to time.(i) - 1 do
      let dist_par = j * (time.(i) - j) in
      if dist_par > dist.(i) then incr c
    done;
    p := !p * !c;
    c := 0
  done;
  !p
