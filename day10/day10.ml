let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let get_ind_dep file =
  let ind = ref (0, 0) in
  file |> Array.iteri (fun i e -> if String.contains e 'S' 
  then ind := (i, String.index_from e 0 'S'));
  !ind

let rec chemin file acc i j prec_ind =
  if file.(i).[j] <> 'S' then (
    if file.(i).[j] = '-' && (i, j + 1) <> prec_ind then chemin file (acc + 1) i (j + 1) (i, j)
    else if file.(i).[j] = '-' && (i, j - 1) <> prec_ind then chemin file (acc + 1) i (j - 1) (i, j)
    else if file.(i).[j] = '|' && (i + 1, j) <> prec_ind then chemin file (acc + 1) (i + 1) j (i, j)
    else if file.(i).[j] = '|' && (i - 1, j) <> prec_ind then chemin file (acc + 1) (i - 1) j (i, j)
    else if file.(i).[j] = 'L' && (i - 1, j) <> prec_ind then chemin file (acc + 1) (i - 1) j (i, j)
    else if file.(i).[j] = 'L' && (i, j + 1) <> prec_ind then chemin file (acc + 1) i (j + 1) (i, j)
    else if file.(i).[j] = '7' && (i, j - 1) <> prec_ind then chemin file (acc + 1) i (j - 1) (i, j)
    else if file.(i).[j] = '7' && (i + 1, j) <> prec_ind then chemin file (acc + 1) (i + 1) j (i ,j)
    else if file.(i).[j] = 'J' && (i, j - 1) <> prec_ind then chemin file (acc + 1) i (j - 1) (i, j)
    else if file.(i).[j] = 'J' && (i - 1, j) <> prec_ind then chemin file (acc + 1) (i - 1) j (i, j)
    else if file.(i).[j] = 'F' && (i, j + 1) <> prec_ind then chemin file (acc + 1) i (j + 1) (i, j)
    else if file.(i).[j] = 'F' && (i + 1, j) <> prec_ind then chemin file (acc + 1) (i + 1) j (i, j))
  else print_int (acc/2)

let new_input file =
  let t = Array.make ((Array.length file) * 3) "" in
  for i = 0 to Array.length file - 1 do
    for j = 0 to String.length file.(i) - 1 do
      match file.(i).[j] with
      | '$' -> t.(i * 3) <- t.(i * 3) ^ "ooo"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "ooo"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "ooo"
      | 'S' -> t.(i * 3) <- t.(i * 3) ^ "@@@"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "@@@"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "@@@"
      | '.' -> t.(i * 3) <- t.(i * 3) ^ "ooo"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "ooo"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "ooo"
      | '|' -> t.(i * 3) <- t.(i * 3) ^ "o@o"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "o@o"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "o@o"
      | '-' -> t.(i * 3) <- t.(i * 3) ^ "ooo"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "@@@"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "ooo"
      | 'J' -> t.(i * 3) <- t.(i * 3) ^ "o@o"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "@@o"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "ooo"
      | 'F' -> t.(i * 3) <- t.(i * 3) ^ "ooo"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "o@@"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "o@o"
      | '7' -> t.(i * 3) <- t.(i * 3) ^ "ooo"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "@@o"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "o@o"
      | 'L' -> t.(i * 3) <- t.(i * 3) ^ "o@o"; t.(i * 3 + 1) <- t.(i * 3 + 1) ^ "o@@"; t.(i * 3 + 2) <- t.(i * 3 + 2) ^ "ooo"
    done;
  done;
  t

let write_list filename output =
  Out_channel.with_open_text filename (fun chan ->
    Array.iter (Printf.fprintf chan "%s\n") output
  )


let main =
  let file = read_lines (open_in "day10.txt") |> Array.of_list |> Array.map (fun e -> "$" ^ e ^ "$") in
  write_list "test.txt" (new_input file)
