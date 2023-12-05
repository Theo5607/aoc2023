let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

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

let main =
  let file = read_lines (open_in "day5.txt") |> Array.of_list in
  res file
