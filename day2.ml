let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []

let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let is_digit c =
  let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|] in
  Array.mem c chiffres

let res tab =
  let c = ref 0 in
  for p = 0 to 99 do
    let flag = ref true in
    let mains = Array.of_list (String.split_on_char ';' tab.(p)) in
    let cols = Array.make 3 0 in
    for i = 0 to Array.length mains - 1 do
      if i <> 0 then mains.(i) <- String.sub mains.(i) 1 (String.length mains.(i) - 1);
      let sets = Array.of_list (String.split_on_char ',' mains.(i)) in
      for j = 0 to Array.length sets - 1 do
        if j <> 0 then sets.(j) <- String.sub sets.(j) 1 (String.length sets.(j) - 1);
        let k = ref 0 in
        let nb = ref "" in
        while is_digit sets.(j).[!k] do
          nb := !nb ^ (String.make 1 sets.(j).[!k]);
          incr k
        done;
        match sets.(j).[!k + 1] with
        | 'r' -> cols.(0) <- max (int_of_string !nb) cols.(0)
        | 'g' -> cols.(1) <- max (int_of_string !nb) cols.(1)
        | 'b' -> cols.(2) <- max (int_of_string !nb) cols.(2)
        | _ -> ();
      done;
    done;
    if !flag then c := !c + cols.(0)*cols.(1)*cols.(2)
  done;
  !c

let main = 
  let fichier = read_lines (open_in "day2.txt") in
  let tab =  Array.of_list fichier in
  for i = 0 to 8 do
    tab.(i) <- String.sub tab.(i) 8 (String.length tab.(i) - 8)
  done;
  for i = 9 to 98 do
    tab.(i) <- String.sub tab.(i) 9 (String.length tab.(i) - 9)
  done;
  tab.(99) <- String.sub tab.(99) 10 (String.length tab.(99) - 10);
  print_int (res tab)
