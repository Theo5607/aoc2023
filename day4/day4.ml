let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []

let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let res tab =
  let c = ref 0 in
  for i = 0 to Array.length tab - 1 do
    let [nb_gagn_lst; nbs_lst] = String.split_on_char '|' tab.(i) in
    let nb_gagn = Array.of_list (List.filter (fun e -> e <> "") (String.split_on_char ' ' nb_gagn_lst)) in
    let nbs = List.filter (fun e -> e <> "") (String.split_on_char ' ' nbs_lst) in
    let pow2 = ref 0 in
    for i = 0 to Array.length nb_gagn - 1 do
      if List.mem nb_gagn.(i) nbs && !pow2 = 0 then pow2 := 1
      else if List.mem nb_gagn.(i) nbs then pow2 := (!pow2) * 2
    done;
    c := !c + !pow2
  done;
  !c

let res2 tab =
  let a = Array.make (Array.length tab) 1 in
  for i = 0 to Array.length tab - 1 do
    let [nb_gagn_lst; nbs_lst] = String.split_on_char '|' tab.(i) in
    let nb_gagn = Array.of_list (List.filter (fun e -> e <> "") (String.split_on_char ' ' nb_gagn_lst)) in
    let nbs = List.filter (fun e -> e <> "") (String.split_on_char ' ' nbs_lst) in
    let c = ref 0 in
    for j = 0 to Array.length nb_gagn - 1 do
      try
      if List.mem nb_gagn.(j) nbs then incr c
      with Invalid_argument _ -> ()
    done;
    for j = 1 to !c do
      a.(i + j) <- a.(i + j) + a.(i)
    done;
  done;
  Array.map (fun e -> print_int e; print_char '\n') a;
  Array.fold_left (+) 0 a



let main = 
  let fichier = read_lines (open_in "day4.txt") in
  let tab = Array.of_list fichier in
  for i = 0 to Array.length tab - 1 do
    tab.(i) <- List.nth (String.split_on_char ':' tab.(i)) 1;
  done;
  print_string "le résultat est : ";
  res2 tab
