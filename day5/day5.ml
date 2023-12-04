let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let () =
  let file = read_lines (open_in "tay1.txt") |> Array.of_list in
  Array.iter print_string file
